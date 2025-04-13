{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TemplateHaskell #-}

import Data.Text (Text)
import Data.Text qualified as Text
import Elm.TyRep
import Servant.API
import Servant.Elm
import TODO.API


taggedIdDefs :: Text -> Text
taggedIdDefs name = "\
  \type " <> name <> " = " <> name <> " UUID.UUID\n\
  \\n\
  \jsonDec" <> name <> " : Json.Decode.Decoder (" <> name <> ")\n\
  \jsonDec" <> name <> " = Json.Decode.map " <> name <> " UUID.jsonDecoder\n\
  \\n\
  \jsonEnc" <> name <> " : (" <> name <> ") -> Value\n\
  \jsonEnc" <> name <> " (" <> name <> " uuid) = UUID.toValue uuid\n\
  \"

taggedIdToString :: Text -> Text
taggedIdToString name = "(\\(" <> name <> " uuid) -> UUID.toString uuid)"

deriveElmDef apiJSON ''ItemBrief
deriveElmDef apiJSON ''Item
deriveElmDef apiJSON ''ItemInsert
deriveElmDef apiJSON ''Tag
deriveElmDef apiJSON ''TagInsert
deriveElmDef apiJSON ''Color

typeAlterations :: EType -> EType
typeAlterations = \case
  ETyApp (ETyCon (ETCon "Maybe")) ty
    -> ETyApp (ETyCon (ETCon "Maybe")) $ typeAlterations ty
  t
    | t == toElmType @NoContent Proxy -> toElmType @() Proxy
    | t == toElmType @ItemId Proxy -> ETyCon (ETCon "ItemId")
    | t == toElmType @TagId Proxy -> ETyCon (ETCon "TagId")
    | otherwise -> defElmOptions.elmTypeAlterations t

main :: IO ()
main = generateElmModuleWith
  defElmOptions
    { elmTypeAlterations = typeAlterations
    , elmToString = \t -> if
      | t == toElmType @ItemId Proxy -> taggedIdToString "ItemId"
      | t == toElmType @TagId Proxy -> taggedIdToString "TagId"
      | otherwise -> defElmOptions.elmToString t
    }
  ["Generated", "API"]
  (Text.unlines $ Text.lines defElmImports <>
    [ "import UUID"
    , taggedIdDefs "ItemId"
    , taggedIdDefs "TagId"
    ])
  "src"
  [ DefineElm @ItemBrief Proxy
  , DefineElm @Item Proxy
  , DefineElm @ItemInsert Proxy
  , DefineElm @Tag Proxy
  , DefineElm @TagInsert Proxy
  , DefineElm @Color Proxy
  ]
  (Proxy @API)
