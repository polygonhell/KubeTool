module Project (Project(..), serviceFromProject, statefulSetFromProject) where

import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics
import Kubernetes
import Environment
import Kubernetes.OpenAPI
import Data.Map (fromList, Map)
import Data.Text (Text, pack)

data Project = Project { name :: !String
                       , template :: !String
                       } deriving (Show, Generic)

instance FromJSON Project
instance ToJSON Project

applicationName :: Text
applicationName = pack "kub"



validateProject :: Environment -> Project -> IO (Either String ())
validateProject env pr = do
  nsExists <- namespaceExists (namespace env) 
  -- Map over the Eithers to get the error or not
  let valid = do 
        nsExists

  return $ valid


labels :: Environment -> Project -> Map String Text
labels env prj = fromList [ ("app", nme)
                          , ("app.kubernetes.io/name", nme)
                          , ("app.kubernetes.io/managed-by", applicationName)
                          ]
  where
    nme = pack $ name prj



statefulSetFromProject :: Environment -> Project -> V1StatefulSet
statefulSetFromProject env pr = mkV1StatefulSet { v1StatefulSetMetadata = Just metadata
                                                , v1StatefulSetSpec = Just spec                                            
                                                }
  where
    nme = pack $ name pr
    labs = labels env pr
    metadata = mkV1ObjectMeta { v1ObjectMetaName = Just nme
                              , v1ObjectMetaNamespace = Just $ pack $ namespace env
                              , v1ObjectMetaLabels = Just labs
                              }
    selector = mkV1LabelSelector  { v1LabelSelectorMatchLabels = Just labs
                                  }

    containerPorts = [mkV1ContainerPort 80]
    runContainer = (mkV1Container (pack "podName")) { v1ContainerImage = Just $ pack "nginx"
                                                    , v1ContainerName = pack "nginx"
                                                    , v1ContainerPorts = Just $ containerPorts
                                                    }
    podSpec = mkV1PodSpec [runContainer]
    podTemplateMeta = mkV1ObjectMeta  { v1ObjectMetaLabels = Just labs
                              } 
    podTemplate = mkV1PodTemplateSpec { v1PodTemplateSpecSpec = Just podSpec 
                                      , v1PodTemplateSpecMetadata = Just podTemplateMeta
                                      }
    spec = mkV1StatefulSetSpec selector nme podTemplate


serviceFromProject :: Environment -> Project -> V1Service
serviceFromProject env proj = mkV1Service { v1ServiceMetadata = Just metadata
                                          , v1ServiceSpec = Just spec
                                          }
  where
    nme = pack $ name proj
    labs = labels env proj
    metadata = mkV1ObjectMeta { v1ObjectMetaName = Just nme
                              , v1ObjectMetaNamespace = Just $ pack $ namespace env
                              , v1ObjectMetaLabels = Just labs
                              }
    port = mkV1ServicePort 80
    spec = mkV1ServiceSpec { v1ServiceSpecPorts = Just [ port ]
                           , v1ServiceSpecSelector = Just labs
                           }
