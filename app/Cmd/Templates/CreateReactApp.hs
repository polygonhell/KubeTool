module Cmd.Templates.CreateReactApp where

createReactApp = unlines [
  "- buildContainer: polygonhell/nodejs-kt:latest",
  "  deployContainer: someOtherURL",
  "  ports:",
  "    - 3000",
  "  runDevCommand: \"npm i && npm run start\"",
  "  sourceFiles:",
  "    - src",
  "    - public",
  "    - package.json",
  "    - package-lock.json",
  "    - tsconfig.json",
  "  name: TSCreateReactApp"
  ]


