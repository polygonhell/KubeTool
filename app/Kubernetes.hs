{-# LANGUAGE OverloadedStrings #-}

module Kubernetes (managedBy, execCmd, copy, getPodsWithName, podName, podLabels, listNamespaces, namespaceName, namespaceExists, createService, createStatefulSet) where

import qualified Kubernetes.OpenAPI.API.CoreV1 as CoreV1
import qualified Kubernetes.OpenAPI.API.AppsV1 as AppsV1
import Kubernetes.OpenAPI
import Kubernetes.OpenAPI.Client (dispatchMime, dispatchLbs, _toInitRequest, modifyInitRequest, dispatchInitUnsafe, InitRequest(InitRequest), MimeResult(MimeResult), MimeError(mimeError))
import Kubernetes.OpenAPI.Core ((-&-), KubernetesRequest(rParams))
import Data.Text (Text, pack, unpack)
import Text.Printf ( printf )
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
import Numeric (showHex)

import KubeConfig ( kubeClientConfig )
import Network.HTTP.Client (Request(..))
import qualified Data.CaseInsensitive as CI

import Network.HTTP.Client ( Request(..), BodyReader)
import qualified Network.URI.Encode as URI
import Network.HTTP.Client.Internal (makeLengthReader, getConn, getResponse, Connection(..))
import Network.HTTP.Client (Response(..), responseClose, responseOpen, withConnection)
import Network.HTTP.Types (Status(..))

import Data.Bits ( Bits(shiftL, (.&.)) )
import Data.Word ( Word16, Word32, Word64 )
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map

import Control.Concurrent ( threadDelay )
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (word64BE, toLazyByteString)
import qualified Codec.Archive.Tar as Tar




hex :: B.ByteString -> String
hex = concatMap ((\x -> "0x"++x++" ") . flip showHex "") . B.unpack



data WSblock = Close | Bin B.ByteString | Unknown

readMore :: Int -> Connection -> IO B.ByteString
readMore l con = 
  if (l == 0) then
    return ""
  else do
    byteStr <- connectionRead con
    let count = B.length byteStr
    if (count < l) then do
      more <- readMore (l - count) con
      return $ B.append byteStr $ more
    else
      return byteStr

readBlock :: Connection -> IO WSblock
readBlock con = do
  byteStr <- connectionRead con 
  let bytes = B.unpack byteStr

  -- putStrLn $ hex byteStr
  -- putStrLn $ C8.unpack byteStr

  let opcode = (head bytes) .&. 0xf

  case opcode of
    2 -> do
      let fin = ((head bytes) .&. 0x80 == 0x80)
      let mask = (bytes !! 1) .&. 0x80
      let l1 = (bytes !! 1) .&. 0x7f
      let (len, bytes') = case l1 of
                          0x7e -> (l, d) where
                            l = fromIntegral $ (((fromIntegral (bytes !! 2)) :: Word16) `shiftL` 8) + 
                                              ((fromIntegral (bytes !! 3)) :: Word16) :: Int
                            d = drop 4 bytes
                          0x7f -> (l, d) where
                            l = fromIntegral $ (((fromIntegral (bytes !! 2)) :: Word32) `shiftL` 24) + 
                                              (((fromIntegral (bytes !! 3)) :: Word32) `shiftL` 16) +
                                              (((fromIntegral (bytes !! 4)) :: Word32) `shiftL` 8) +
                                              ((fromIntegral (bytes !! 5)) :: Word32) :: Int
                            d = drop 6 bytes
                          _ -> (fromIntegral l1, drop 2 bytes)

      -- putStrLn $ printf "Reading len %d bytes from data containing %d bytes" len (length bytes')
      moreBytes <- readMore (len - length bytes') con
      -- putStrLn $ printf "Read additional %d bytes" (B.length moreBytes)

      let dataBytes = B.append (B.pack bytes') moreBytes
      -- putStrLn $ printf "Reading len %d bytes from data containing %d bytes" len (B.length dataBytes)

      let toReturn = B.drop len dataBytes
      -- putStrLn $ printf "Returning %d bytes to connection" (B.length toReturn)
      if (B.length toReturn > 0) then connectionUnread con $ B.drop len dataBytes else return ()

      let resBytes = B.take len dataBytes

      -- putStrLn $ hex resBytes

      case fin of
        True -> return $ Bin resBytes
        False -> do
          Bin moreBytes <- readBlock con
          return $ Bin $ B.append resBytes moreBytes
    8 -> return Close


getOutput :: Connection -> IO B.ByteString
getOutput con = do
  foo <- readBlock con
  case foo of
    Close -> do
      putStrLn "Closing connection"
      return B.empty
    Bin bits -> do
      -- first byte designates the output channel
      moreBits <- getOutput con
      return $ B.append (B.tail bits) moreBits
    _  -> error "unsupported packet seen"
  

-- Kubernetes library doesn't support the change to WebSockets, so we have to do  it the hardware
-- Other issues - with Command optional arg not allowing multiple instantiations
execCmd :: String -> String -> [String] -> IO (Either String String)
execCmd namespaceStr podName cmd = do
  putStrLn "execCmd"
  (mgr, kcfg) <- kubeClientConfig
  let name = Name (pack podName)
  let namespace = Namespace (pack namespaceStr)

  let request =  (CoreV1.connectGetNamespacedPodExec (Accept MimeAny) name namespace) -&- (Stdout True) -&- (Stderr True)
  let addHeaders r = r { 
      requestHeaders = requestHeaders r ++ [
        (CI.mk "Host", host r),
        (CI.mk "Sec-WebSocket-Version", "13"),
        (CI.mk "Sec-WebSocket-Key", "SGVsbG8sIHdvcmxkIQ=="),
        (CI.mk "Connection", "Upgrade"),
        (CI.mk "Upgrade", "websocket")
      ],
      -- add the command to the query string
      queryString = B.append (queryString r) $ toCommandQuery cmd
    }

  initRequest' <- _toInitRequest kcfg request
  let initRequest = modifyInitRequest initRequest' addHeaders
  let (InitRequest rq) = initRequest

  resp <- responseOpen rq mgr
  -- Check we get the expected 101
  res <- case responseStatus resp of
              Status 101 _ -> do
                -- Pull the stdout/error bytes off the wire
                out <- withConnection rq mgr getOutput
                return $ Right $ C8.unpack out
              status -> do
                print status
                return $ Left $ printf "Error in Exec -- %s" (show status)

  responseClose resp
  return $ res
  where
    toCommandQuery :: [String] -> B.ByteString
    toCommandQuery cs = C8.pack $ foldl toCommandQueryPart "" cs
    toCommandQueryPart :: String -> String -> String
    toCommandQueryPart a b = a ++ printf "&command=%s" (URI.encode b)


managedBy :: String
managedBy="kc"

-- Get the pod associated with a projects deployment
getPodsWithName :: String -> String -> IO (Either String [V1Pod])
getPodsWithName namespaceStr projectName = do
  (mgr, kcfg) <- kubeClientConfig
  let namespace = Namespace (pack namespaceStr)
  let selectorText = pack $ printf "app.kubernetes.io/name=%s,app.kubernetes.io/managed-by=%s" projectName managedBy
  let request =  (CoreV1.listNamespacedPod (Accept MimeJSON) namespace) -&- (LabelSelector selectorText)
  res0 <- dispatchMime mgr kcfg request 
  let res = case res0 of
              MimeResult (Left err) _ -> Left $ mimeError err
              MimeResult (Right (V1PodList { v1PodListItems=pods })) _ -> Right pods

  return $ res


podName :: V1Pod -> String
podName p = fromJust $ do 
  meta <- v1PodMetadata p
  name <- v1ObjectMetaName meta
  return $ unpack name

podLabels :: V1Pod -> (Map.Map String String)
podLabels p = fromMaybe Map.empty $ do
  meta <- v1PodMetadata p
  labels <- v1ObjectMetaLabels meta
  return $ Map.map (\b -> unpack b) labels
  


-- TODO Copy Files - Done with exec

copy :: String -> String -> String -> [String] -> String -> IO (Either String String)
copy namespaceStr podName baseDir files dest = do
  putStrLn "copy"
  (mgr, kcfg) <- kubeClientConfig
  let name = Name (pack podName)
  let namespace = Namespace (pack namespaceStr)

  let request =  (CoreV1.connectGetNamespacedPodExec (Accept MimeAny) name namespace) -&- (Stdout True) -&- (Stderr True) -&- (Stdin True) -&- (Tty False)
  let addHeaders r = r { 
      requestHeaders = requestHeaders r ++ [
        (CI.mk "Host", host r),
        (CI.mk "Sec-WebSocket-Version", "13"),
        (CI.mk "Sec-WebSocket-Key", "SGVsbG8sIHdvcmxkIQ=="),
        (CI.mk "Connection", "Upgrade"),
        (CI.mk "Upgrade", "websocket")
      ],
      -- add the command to the query string
      queryString = B.append (queryString r) $ toCommandQuery ["tar", "xvmf", "-", "-C", dest]
    }

  initRequest' <- _toInitRequest kcfg request
  let initRequest = modifyInitRequest initRequest' addHeaders
  let (InitRequest rq) = initRequest

  resp <- responseOpen rq mgr

  -- Create a tar file from the requested files
  entries <- Tar.pack baseDir files
  let bytes = toStrict $ Tar.write entries
  -- Wrap in a websocket request and prepend 00 to designate stdin
  let lenBs = toStrict $ toLazyByteString $ word64BE (fromIntegral (B.length bytes + 1) :: Word64)
  let b3 = B.concat [B.pack [0x82, 0xFF], lenBs, B.pack [0x00, 0x00, 0x00, 0x00, 0x00], bytes]

  -- let b3 = (B.append (B.pack [0x82, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x81, 0x00, 0x00, 0x00, 0x00, 0x00])  bytes) 
  let b4 = (B.pack [0x88, 0x00])

  -- Check we get the expected 101
  res <- case responseStatus resp of
              Status 101 _ -> do
                out <- withConnection rq mgr (\con -> do readBlock con ; connectionWrite con b3 ; connectionWrite con b4 ; getOutput con )
                -- Pull the stdout/error bytes off the wire
                -- out <- withConnection rq mgr getOutput
                return $ Right $ C8.unpack out
              status -> do
                print status
                return $ Left $ printf "Error in Exec -- %s" (show status)

  responseClose resp
  return $ res
  where
    toCommandQuery :: [String] -> B.ByteString
    toCommandQuery cs = C8.pack $ foldl toCommandQueryPart "" cs
    toCommandQueryPart :: String -> String -> String
    toCommandQueryPart a b = a ++ printf "&command=%s" (URI.encode b)


listNamespaces :: IO (Either String [V1Namespace])
listNamespaces = do
  (mgr, kcfg) <- kubeClientConfig
  res0 <- dispatchMime mgr kcfg (CoreV1.listNamespace (Accept MimeJSON))
  let res = case res0 of
            MimeResult (Left err) _ -> Left $ mimeError err
            MimeResult (Right (V1NamespaceList { v1NamespaceListItems=namespaces })) _ -> Right namespaces
  return res

namespaceName :: V1Namespace -> String
namespaceName p = fromJust $ do 
  meta <- v1NamespaceMetadata p
  name <- v1ObjectMetaName meta
  return $ unpack name


namespaceExists :: String -> IO (Either String ())
namespaceExists name = do
  ns' <- listNamespaces
  let exists = do
              ns <- ns'
              let names = map namespaceName ns
              if (elem name names) then return True else return False
  case exists of
    Right True -> return $ Right ()
    Right False -> return $ Left $ (printf "Namespace %s does not exist" name :: String)
    Left e -> return $ Left e




createStatefulSet :: String -> V1StatefulSet -> IO (Either String (V1StatefulSet))
createStatefulSet ns ss = do
  (mgr, kcfg) <- kubeClientConfig
  let namespace = Namespace $ pack ns
  res0 <- dispatchMime mgr kcfg (AppsV1.createNamespacedStatefulSet (ContentType MimeJSON) (Accept MimeJSON) ss namespace)
  print res0
  let res = case res0 of
        MimeResult (Left err) _ -> Left $ mimeError err
        MimeResult (Right v1s) _ -> Right v1s
  return res

createService :: String -> V1Service -> IO (Either String (V1Service))
createService ns service = do
  (mgr, kcfg) <- kubeClientConfig
  let namespace = Namespace $ pack ns
  res0 <- dispatchMime mgr kcfg (CoreV1.createNamespacedService (ContentType MimeJSON) (Accept MimeJSON) service namespace)
  print res0
  let res = case res0 of
          MimeResult (Left err) _ -> Left $ mimeError err
          MimeResult (Right v1s) _ -> Right v1s
  return res
