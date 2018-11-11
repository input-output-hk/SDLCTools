{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Meas.Dev.Parser
()
where

-- import Debug.Trace (trace)

import            Data.Aeson
import            Data.Aeson.Types (Parser)
import            Data.Either
import qualified  Data.List as L
import            Data.Maybe (catMaybes)
import qualified  Data.Text as T
import            Data.Text.Conversions
import qualified  Data.Vector as V

import            Text.Read (readMaybe)


import            Meas.Dev.Types

head' :: [Char] -> [p] -> p
head' msg [] = error $ "head2 : empty list + -> "++msg
head' _ l = head l


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

genericIssueParser :: Value -> Parser GenericIssue
genericIssueParser =
  withObject "Issue" $ \o -> do
    attrs <- o .: "attrs"
    issueId <- attrs .: "id"
    fieldsVal <- o .: "items"
    fields <- issueFieldsParser fieldsVal
    return $ MkGenericIssue issueId fields

issueFieldsParser :: Value -> Parser [GenericIssueField]
issueFieldsParser value = do
  res <-  withArray "field items" (mapM issueFieldParser . V.toList) value
  return $ catMaybes res

issueLinkParser :: Object -> Parser (Maybe GenericIssueField)
issueLinkParser o = do
  itemsVal <- o .: "items"
  withArray "link elements" (mapM p . V.toList) itemsVal >>= (return . Just . GLinkField . L.map (\(l, t) -> (fromText l, t)))
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
          "updated"           -> issueSimpleFieldParser GUpdatedField o
          "summary"           -> issueSimpleFieldWithDefaultParser T.empty GSummaryField o
          "description"       -> issueSimpleFieldWithDefaultParser T.empty GDescriptionField o
          "Type"              -> issueSimpleFieldParser GTypeField o
          "State"             -> issueEnumFieldParser GStateField o
          "Wait"              -> issueEnumFieldParser GWaitField o
          "Assignee"          -> issueSimpleFieldParser GAssigneeField o
          "3D"                -> issueEnumFieldParser GThreeDField o
          "ROM Mandays"       -> issueEnumFieldParser GRomManDaysField o
          "Squad"             -> issueEnumFieldParser GSquadField o
          "Owner"             -> issueSimpleFieldParser GOwnerField o
          "Due Date"          -> issueSimpleFieldParser GDueDateField o
          "p_easy"            -> issueEnumFieldParser GPEasyField o
          "p_benefits"        -> issueEnumFieldParser GPBenefitsField o
          "p_urgency"         -> issueEnumFieldParser GPUrgencyField o
          "Target versions"   -> issueSimpleFieldParser GTargetVersionsField o
          "Potential Squads"  -> issueSimpleFieldParser GPotentialSquadField o
          "Resolution"        -> issueSimpleFieldParser GResolutionField o

          "links"             -> issueLinkParser o
          _                   -> return Nothing
      _ -> return Nothing


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

issueSimpleFieldWithDefaultParser :: T.Text -> (T.Text -> a) -> Object -> Parser (Maybe a)
issueSimpleFieldWithDefaultParser def ctor o = do
  itemsVal <- o .: "items"
  value <- parseNamedSingletonArray p itemsVal
  return $ Just $ ctor value
  where
  p :: Value -> Parser (Maybe T.Text)
  p = withObject "" $ \o' -> do
        (name::T.Text) <- o' .: "name"
        case name of
          "value" -> do
            itemsVal <- o' .: "items"
            parseSingletonArrayWithDefault def (withText "field value" pure) itemsVal  >>= (return . Just)
          _ -> return Nothing



-- improve
parseSingletonArray :: (Value -> Parser a) -> Value -> Parser a
parseSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . (head' "a"))


parseSingletonArrayWithDefault :: a -> (Value -> Parser a) -> Value -> Parser a
parseSingletonArrayWithDefault def p value =
  withArray "Singleton Array" (\a -> do
    case V.toList a of
      [] -> return def
      [s] -> p s
      _ -> error "Cannot parse: array contains more than 1 element") value


parseNamedSingletonArray :: (Value -> Parser (Maybe a)) -> Value -> Parser a
parseNamedSingletonArray p value =
  withArray "Singleton Array" (\a -> mapM p $ V.toList a) value >>= (return . (head' "b") . catMaybes)


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
            "State" -> (parseFieldChange (\o' n -> StateChange (fromText o') (fromText n)) itemsVal) >>= (return . Just)
            "Wait" -> (parseFieldChange (\o' n -> WaitChange (fromText o') (fromText n)) itemsVal) >>= (return . Just)
            _ -> return Nothing
        _ -> return Nothing



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



instance FromText StateValue where
  fromText "Backlog"                            = Backlog
  fromText "Planning"                           = Planning
  fromText "Selected"                           = Selected
  fromText "In Progress"                        = InProgress
  fromText "Review"                             = Review
  fromText "Done"                               = Done
  fromText "Backlog (pool of Ideas)"            = Backlog
  fromText "To Verify"                          = Backlog

-- old stuff in YouTrack ...
  fromText "Closed"                             = Done
  fromText "No State"                           = Backlog
  fromText "No state"                           = Backlog
  fromText "Open"                               = Backlog
  fromText "Assigned"                           = Selected
  fromText "No value"                           = Backlog
  fromText "Waiting for review"                 = Review
  fromText "DONT-USE-1"                         = Backlog
  fromText "DONT-USE-3"                         = Backlog
  fromText "DONT-USE-5"                         = Backlog
  fromText "Aborted"                            = Done
  fromText "To be discussed"                    = Backlog
  fromText "Waiting for build"                  = Backlog
  fromText "79-532-1524688761506.Done"          = Done
  fromText "79-532-1524688761506.Backlog"       = Backlog
  fromText "79-532-1524688761506.In Progress"   = InProgress
  fromText "Duplicate"                          = Done
  fromText "Verified"                           = Done
  fromText "Pool of Ideas"                      = Backlog
  fromText "Obsolete"                           = Done
  fromText "Submitted"                          = Backlog
  fromText "Can't Reproduce"                    = Done
  fromText "Blocking"                           = Done
  fromText "Postponed"                          = Backlog
  fromText "Waiting for test"                   = InProgress
  fromText "Waiting to be merged into `master`" = InProgress
  fromText "To be deployed to staging"          = InProgress
  fromText "To be deployed to production"       = InProgress
  fromText "Ready to Solve"                     = Selected
  fromText "Ready"                              = Selected
  fromText "Blocked"                            = Neutral
  fromText "Merged"                             = Done
  fromText "Cancel"                             = Done
  fromText "Close"                              = Done

  fromText s                                    = error $ ("Unknow State: "++T.unpack s)



instance FromText ROMMandaysValue where
  fromText "Day"      = Days
  fromText "Week"     = Weeks
  fromText "Month"    = Months
  fromText "Quarter"  = Quarters
  fromText s          = error ("Unknown ROMMandays: " ++ T.unpack s)



instance FromText WaitValue where
  fromText "Running"        = Running
  fromText "Waiting"        = Waiting
  fromText "No wait"        = Running
  fromText "<lost change>"  = Running
  fromText s                = error $ ("Unknow Wait: "++ T.unpack s)



instance FromText TypeValue where
  fromText "User Story" = IssueType
  fromText "Bug"        = IssueType
  fromText "Task"       = TaskType
  fromText "Issue"      = IssueType
  fromText _            = OtherType



instance FromText ThreeDValue where
  fromText "Design"         = Design
  fromText "Development"    = Development
  fromText "Documentation"  = Documentation
  fromText "Test"           = Test
  fromText s                = error ("Unknown 3D: " ++ T.unpack s)



instance FromText LinkType where
  fromText "parent for"             = ParentFor
  fromText "subtask of"             = SubTaskOf
  fromText "must start after"       = MustStartAfter
  fromText "is a prerequisite for"  = IsPreRequisiteFor
  fromText "depends on"             = DependsOn
  fromText "duplicates"             = Duplicates
  fromText "relates to"             = RelatesTo
  fromText "is duplicated by"       = IsDuplicatedBy
  fromText "is required for"        = IsRequiredFor
  fromText "tested by"              = TestedBy
  fromText "test of"                = TestOf
  fromText s                        = error ("unknow link role: "++T.unpack s)


instance FromText PriorityValue where
  fromText "Show-stopper" = ShowStopper
  fromText "Critical"     = Critical
  fromText "Major"        = Major
  fromText "Normal"       = Normal
  fromText "Minor"        = Minor
  fromText s              = error ("unknow Priority :"++T.unpack s)



instance FromText ResolutionValue where
  fromText "Successful" = Successful
  fromText "Aborted"    = Aborted
  fromText "Duplicate"  = Duplicate
  fromText "Obsolete"   = Obsolete
  fromText s            = error ("unknow Resolution: "++T.unpack s)



