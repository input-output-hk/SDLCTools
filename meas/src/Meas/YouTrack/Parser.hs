{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE TemplateHaskell #-}

module Meas.YouTrack.Parser
where

import Debug.Trace (trace)

import            Control.DeepSeq

import            Data.Aeson
import            Data.Aeson.Types (Parser)
import            Data.Either
import qualified  Data.List as L
import            Data.Maybe (catMaybes)
import qualified  Data.Text as T
import qualified  Data.Text.Conversions as T
import qualified  Data.Vector as V

import            GHC.Generics (Generic)

import            Text.Read (readMaybe)


import            Meas.Extract.Types


data GenericIssues = GenericIssues [GenericIssue]
  deriving (Show, Generic, NFData)


data GenericIssue = MkGenericIssue
  { issueId :: T.Text
  , issueFields :: [GenericIssueField]
  }
  deriving (Show, Generic, NFData)




data GenericIssueField =
  GProjectField T.Text
  |GTypeField T.Text
  |GCreatedField T.Text
  |GNumberField T.Text
  |GStateField T.Text
  |GWaitField T.Text
  |GThreeDField T.Text
  |GDescriptionField T.Text
  |GDueDateField T.Text
  |GRomManDaysField T.Text
  |GSquadField T.Text
  |GOwnerField T.Text
  |GPEasyField T.Text
  |GPBenefitsField T.Text
  |GPUrgencyField T.Text
  |GLinkField [(LinkType, T.Text)]
  |GAssigneeField T.Text
  |GPotentialSquadField T.Text
  |GPriorityField T.Text
  |GSubSystemField T.Text
  |GFixVerionsField T.Text
  |GAffectedVerionsField T.Text
  |GExchangeField T.Text
  |GResolutionField T.Text
  |GPlatformField T.Text
  |GTargetVersions T.Text

  deriving (Show, Generic, NFData)


data Hist =  Hist
  { issue :: Issue
  , changes :: [[ValueChange]]
  }
  deriving (Show, Generic, NFData)

data Issue = Issue T.Text
  deriving (Show, Generic, NFData)



instance FromJSON Hist where
  parseJSON = withObject "hist" $ \o -> do
    topItems <-  (o .: "items") >>= withArray "top items" (mapM topItemParser . V.toList)
    let [issue] = rights topItems
    let valueChanges = lefts topItems
    return $ Hist issue valueChanges

instance FromJSON GenericIssues where
  parseJSON = genericIssuesParser


instance FromJSON GenericIssue where
  parseJSON = genericIssueParser

genericIssuesParser :: Value -> Parser GenericIssues
genericIssuesParser =
  withObject "Issues" $ \o -> do
    issueListVal <- o .: "items"
    issues <- withArray "Issue List" (mapM genericIssueParser . V.toList) issueListVal
    return $ GenericIssues issues
--  where
--  p = withObject "Issue" $ \o -> do
--        attrs <- o .: "attrs"
--        issueId <- attrs .: "id"
--        fieldsVal <- o .: "items"
--        fields <- issueFieldsParser fieldsVal
--        let !evFields = force fields
--        return $ MkGenericIssue issueId evFields

genericIssueParser :: Value -> Parser GenericIssue
genericIssueParser =
  withObject "Issue" $ \o -> do
    attrs <- o .: "attrs"
    issueId <- attrs .: "id"
    fieldsVal <- o .: "items"
    fields <- issueFieldsParser fieldsVal
    let !evFields = force fields
    return $ MkGenericIssue issueId evFields

issueFieldsParser :: Value -> Parser [GenericIssueField]
issueFieldsParser value = do
  res <-  withArray "field items" (mapM issueFieldParser . V.toList) value
  return $ catMaybes res

issueLinkParser :: Object -> Parser (Maybe GenericIssueField)
issueLinkParser o = do
  itemsVal <- o .: "items"
  withArray "link elements" (mapM p . V.toList) itemsVal >>= (return . Just . GLinkField . L.map (\(l, t) -> (T.fromText l, t)))
  where
  p = withObject "link" $ \o' -> do
        attrs <- o' .: "attrs"
        role <- attrs .: "role"
        issueId <- (o' .: "items") >>= parseSingletonArray (withText "field value" pure)
        return (role, issueId)

issueFieldParser :: Value -> Parser (Maybe GenericIssueField)
issueFieldParser =
  withObject "Field" $ \o -> do
    kind <- o .: "name"
    case T.unpack kind of
      "field" -> do
        attrs <- (o .: "attrs")
        name <- attrs .: "name"
        case T.unpack name of
          "projectShortName"  -> issueSimpleFieldParser GProjectField o
          "numberInProject"   -> issueSimpleFieldParser GNumberField o
          "created"           -> issueSimpleFieldParser GCreatedField o
          "Type"              -> issueSimpleFieldParser GTypeField o
          "State"             -> issueEnumFieldParser GStateField o
          "Wait"              -> issueEnumFieldParser GWaitField o
          "Assignee"          -> issueSimpleFieldParser GAssigneeField o
         -- "description" -> issueSimpleFieldParser DescriptionField o
          "3D"                -> issueEnumFieldParser GThreeDField o
          "ROM Mandays"       -> issueEnumFieldParser GRomManDaysField o
          "Squad"             -> issueEnumFieldParser GSquadField o
          "Owner"             -> issueSimpleFieldParser GOwnerField o
          "Due Date"          -> trace (show o) $ issueSimpleFieldParser GDueDateField o
          "p_easy"            -> issueEnumFieldParser GPEasyField o
          "p_benefits"        -> issueEnumFieldParser GPBenefitsField o
          "p_urgency"         -> issueEnumFieldParser GPUrgencyField o
          "Target versions"   -> issueSimpleFieldParser GTargetVersions o

          --IOHKS
          "Subsystem"         -> issueSimpleFieldParser GSubSystemField o
          "Fix versions"      -> issueSimpleFieldParser GFixVerionsField o
          "Affected versions" -> issueSimpleFieldParser GAffectedVerionsField o
          "Exchange"          -> issueSimpleFieldParser GExchangeField o
          "Resolution"        -> issueSimpleFieldParser GResolutionField o
          "Platform"          -> issueSimpleFieldParser GPlatformField o

          "links"             -> issueLinkParser o
          _                   -> return Nothing
      _ -> return Nothing

{-

parseSingletonArray :: (Value -> Parser a) -> Value -> Parser a
parseSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head)
  -}

issueSimpleFieldParser :: (T.Text -> a) -> Object -> Parser (Maybe a)
issueSimpleFieldParser ctor o = do
  itemsVal <- o .: "items"
  value <- parseNamedSingletonArray p itemsVal
  return $ Just $ ctor value
  where
  p = withObject "" $ \o' -> do
        (name::T.Text) <- o' .: "name"
        case name of
          "value" -> do
            itemsVal <- o' .: "items"
            parseSingletonArray (withText "field value" pure) itemsVal >>= (return . Just)
          _ -> return Nothing

issueEnumFieldParser :: (T.Text -> a) -> Object -> Parser (Maybe a)
issueEnumFieldParser ctor o = do
  itemsVal <- o .: "items"
  [value] <- withArray "" (mapM p . V.toList) itemsVal >>= (return . catMaybes)
  return $ Just $ ctor value
  where
  p = withObject "" $ \o' -> do
        name <- o' .: "name"
        case T.unpack name of
          "value" -> do
            itemsVal <- o' .: "items"
            parseSingletonArray (withText "field value" pure) itemsVal >>= (return . Just)
          _ -> return Nothing






-- improve
parseSingletonArray :: (Value -> Parser a) -> Value -> Parser a
parseSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head)


parseNamedSingletonArray :: (Value -> Parser (Maybe a)) -> Value -> Parser a
parseNamedSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . L.head . catMaybes)


parseUpdateTime :: Value -> Parser ValueChange
parseUpdateTime = do
  withObject "updated object" $ \o -> do
    itemsVal <- o .: "items"
    res <- parseSingletonArray p itemsVal
    return $ UpdateTime res
  where
  p = withText "update time" $ \s ->
        case readMaybe (T.unpack s) of
          Just n -> return n
          Nothing -> fail "not a number"

parseFieldChange :: (T.Text -> T.Text -> ValueChange) -> Value -> Parser ValueChange
parseFieldChange ctor value = do
    [Left oldVal, Right newVal] <- withArray "changes" (mapM p . V.toList) value
    return $ ctor oldVal newVal
  where
  p = withObject "kind of value" $ \o -> do
    name <- o .: "name"
    case T.unpack name of
      "oldValue" -> (o .: "items") >>= parseSingletonArray (withText "old value" pure) >>= (return . Left)
      "newValue" -> (o .: "items") >>= parseSingletonArray (withText "new value" pure) >>= (return . Right)
      _ -> fail "neither oldValue nor newValue"


parseUpdaterName :: Value -> Parser ValueChange
parseUpdaterName  = do
  withObject "updater name object" $ \o -> do
    itemsVal <- o .: "items"
    res <- parseSingletonArray  p itemsVal
    return $ Updater res
  where
  p v = withText "updater name" pure v

parseChangeGroup :: Value -> Parser [ValueChange]
parseChangeGroup value = do
  changes <- withArray "items: change group" (mapM singleChangeParser . V.toList) value
  return $ catMaybes changes
  where
  singleChangeParser =
    withObject "Single Change" $ \o -> do
      (name::T.Text) <- o .: "name"
      case name of
        "field" -> do
          attrsVal <- o .: "attrs"
          name' <- withObject "attrs" (\attrs -> attrs .: "name") attrsVal
          (itemsVal :: Value) <- o .: "items"
          case T.unpack name' of
            "updaterName" -> parseSingletonArray parseUpdaterName itemsVal >>= (return . Just)
            "updated" -> parseSingletonArray parseUpdateTime itemsVal >>= (return . Just)
            "State" -> (parseFieldChange (\o' n -> StateChange (T.fromText o') (T.fromText n)) itemsVal) >>= (return . Just)
            "Wait" -> (parseFieldChange (\o' n -> WaitChange (T.fromText o') (T.fromText n)) itemsVal) >>= (return . Just)
            _ -> return Nothing
        _ -> return Nothing


topItemsParser :: Value -> Parser [Either [ValueChange] Issue]
topItemsParser = withArray "items" (mapM topItemParser . V.toList)

parseIssue :: Object -> Parser Issue
parseIssue o = do
  issueId <- (o .: "attrs") >>= (
                withObject "Issue Attrs" $ \o' -> do
                   o' .: "id"
                )
  return $ Issue issueId

topItemParser :: Value -> Parser (Either [ValueChange] Issue)
topItemParser =
  withObject "Top item" $ \o -> do

    name <- o .: "name"
    case T.unpack name of
      "issue" -> parseIssue o >>= (return . Right)
      "change" -> do
        itemsVal <- o .: "items"
        parseChangeGroup itemsVal >>= (return . Left)
      _ -> fail "topItemParser"

