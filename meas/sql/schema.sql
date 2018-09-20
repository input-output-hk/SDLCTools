

DROP DATABASE IF EXISTS sdlc_db;
CREATE DATABASE sdlc_db;
\c sdlc_db;

DROP TABLE IF EXISTS input_static_priorityValueDomain;
DROP TABLE IF EXISTS input_static_iohksStateValueDomain;
DROP TABLE IF EXISTS input_static_stateValueDomain;
DROP TABLE IF EXISTS input_static_waitValueDomain;
DROP TABLE IF EXISTS input_static_typeValueDomain;
DROP TABLE IF EXISTS input_static_threeDValueDomain;
DROP TABLE IF EXISTS input_static_romManDaysValueDomain;
DROP TABLE IF EXISTS input_static_resolutionValueDomain;


CREATE TABLE input_static_priorityValueDomain (
  priorityValue text,
  CONSTRAINT PKC_input_static_priorityValueDomain PRIMARY KEY (priorityValue)
);

insert into input_static_priorityValueDomain values
    ('ShowStopper')
  , ('Critical')
  , ('Major')
  , ('Normal')
  , ('Minor');

CREATE TABLE input_static_iohksStateValueDomain (
  iohksStateValue text,
  CONSTRAINT PKC_input_static_iohksStateValueDomain PRIMARY KEY (iohksStateValue)
);

insert into input_static_iohksStateValueDomain values
    ('IohksSubmitted')
  , ('IohksReadyToSolve')
  , ('IohksFixed')
  , ('IohksDone');

CREATE TABLE input_static_stateValueDomain (
  stateValue text,
  CONSTRAINT PKC_input_static_stateValueDomain PRIMARY KEY (stateValue)
);

insert into input_static_stateValueDomain values
    ('Backlog')
  , ('Planning')
  , ('Selected')
  , ('InProgress')
  , ('Review')
  , ('Done');

CREATE TABLE input_static_waitValueDomain (
  waitValue text,
  CONSTRAINT PKC_input_static_waitValueDomain PRIMARY KEY (waitValue)
);

insert into input_static_waitValueDomain values
    ('Running')
  , ('Waiting');


CREATE TABLE input_static_typeValueDomain (
  typeValue text,
  CONSTRAINT PKC_input_static_typeValueDomain PRIMARY KEY (typeValue)
);

insert into input_static_typeValueDomain values
    ('TaskType')
  , ('IssueType')
  , ('OtherType');


CREATE TABLE input_static_threeDValueDomain (
  threeDValue text,
  CONSTRAINT PKC_input_static_threeDValueDomain PRIMARY KEY (threeDValue)
);

insert into input_static_threeDValueDomain values
    ('Design')
  , ('Development')
  , ('Documentation')
  , ('Test');

CREATE TABLE input_static_romManDaysValueDomain (
  romManDaysValue text,
  CONSTRAINT PKC_input_static_romManDaysValueDomain PRIMARY KEY (romManDaysValue)
);

insert into input_static_romManDaysValueDomain values
    ('Days')
  , ('Weeks')
  , ('Months')
  , ('Quarters');

CREATE TABLE input_static_resolutionValueDomain (
  resolutionValue text,
  CONSTRAINT PKC_input_static_resolutionValueDomain PRIMARY KEY (resolutionValue)
);

insert into input_static_resolutionValueDomain values
    ('Successful')
  , ('Aborted')
  , ('Duplicate')
  , ('Obsolete');

CREATE TABLE input_static_linkTypeDomain (
  linkType text,
  CONSTRAINT PKC_input_static_linkTypeDomain PRIMARY KEY (linkType)
);

insert into input_static_linkTypeDomain values
    ('ParentFor')
  , ('SubTaskOf')
  , ('MustStartAfter')
  , ('IsPreRequisiteFor')
  , ('DependsOn')
  , ('Duplicates')
  , ('RelatesTo')
  , ('IsDuplicatedBy')
  , ('IsRequiredFor');

CREATE TABLE input_static_valueChangeTypeDomain (
  valueChangeType text,
  CONSTRAINT PKC_input_static_valueChangeTypeDomain PRIMARY KEY (valueChangeType)
);

insert into input_static_valueChangeTypeDomain values
    ('Time Updated')
  , ('Updated By')
  , ('State Changed')
  , ('Wait Changed');

CREATE TABLE input_dynamic_developers (
  developerName text NOT NULL,
  otherDetails text,
  CONSTRAINT PKC_input_dynamic_developers PRIMARY KEY (developerName)
);

CREATE TABLE input_dynamic_squads (
  squadId Integer NOT NULL,
  squadLead text NOT NULL,
  squadSize Integer NOT NULL,
  CONSTRAINT PKC_input_dynamic_squads PRIMARY KEY (squadId),
  FOREIGN KEY (squadLead) REFERENCES input_dynamic_developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE input_dynamic_squadDetails (
  squadId Integer NOT NULL,
  squadMember text NOT NULL,
  CONSTRAINT PKC_input_dynamic_squadDetails PRIMARY KEY (squadId,squadMember),
  FOREIGN KEY (squadId) REFERENCES input_dynamic_squads (squadId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (squadMember) REFERENCES input_dynamic_developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE input_dynamic_targetVersionDomain (
  targetVersion text NOT NULL,
  CONSTRAINT PKC_input_dynamic_targetVersionDomain PRIMARY KEY (targetVersion)
);

CREATE TABLE aux_dynamic_targetVersionGroups (
  targetVersionGroupId Integer NOT NULL,
  CONSTRAINT PKC_input_dynamic_targetVersionGroups PRIMARY KEY (targetVersionGroupId)
);

CREATE TABLE input_dynamic_targetVersionGroupDetails (
  targetVersionGroupId Integer NOT NULL,
  targetVersion text NOT NULL,
  CONSTRAINT PKC_input_dynamic_targetVersionGroupDetails PRIMARY KEY (targetVersionGroupId, targetVersion),
  FOREIGN KEY (targetVersionGroupId) REFERENCES aux_dynamic_targetVersionGroups (targetVersionGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (targetVersion) REFERENCES input_dynamic_targetVersionDomain (targetVersion)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE aux_dynamic_linkGroups (
  linkGroupId Integer NOT NULL,
  CONSTRAINT PKC_input_dynamic_linkGroups PRIMARY KEY (linkGroupId)
);

CREATE TABLE input_dynamic_linkGroupDetails (
  linkGroupId Integer NOT NULL,
  linkType text NOT NULL,
  linkedTicketId text NOT NULL,
  CONSTRAINT PKC_input_dynamic_linkGroupDetails PRIMARY KEY (linkGroupId,linkType,linkedTicketId),
  FOREIGN KEY (linkGroupId) REFERENCES aux_dynamic_linkGroups (linkGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (linkType) REFERENCES input_static_linkTypeDomain (linkType)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE input_dynamic_ytgErrors (
  ytgTicketId text NOT NULL,
  ytError     text NOT NULL,
  CONSTRAINT PKC_input_dynamic_ytgErrors PRIMARY KEY (ytgTicketId,ytError)
  -- later to add FK 
);

CREATE TABLE input_dynamic_ytIssueDetails (
  ytiIssueId text NOT NULL,
  ytiType    text NOT NULL,
  ytiSummary text NOT NULL,
  ytiDescription text NOT NULL,
  ytiCreated Integer NOT NULL,
  ytiUpdatedAt Integer,
  ytiProject text NOT NULL,
  ytiNumber  Integer NOT NULL,
  ytiState   text NOT NULL,
  ytiWait    text NOT NULL,
  ytiDueDate Integer NOT NULL,
  ytiROMManDay text,
  ytiPriority1 Integer NOT NULL,
  ytiPriority2 Integer NOT NULL,
  ytiPriority3 Integer NOT NULL,
  ytiSquadId Integer,
  ytiTargetVersionGroupId Integer,
  ytiowner   text,
  ytiResolution text NOT NULL,
  ytiLinkGroupId Integer,
  ytiStateTransitionId Integer NOT NULL,     -- todo : to add tables related to this
  ytiBlockedDays Integer NOT NULL,
  CONSTRAINT PKC_input_dynamic_YtIssueDetails PRIMARY KEY (ytiIssueId),
  FOREIGN KEY (ytiType) REFERENCES input_static_typeValueDomain (typeValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiState) REFERENCES input_static_stateValueDomain (stateValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiWait) REFERENCES input_static_waitValueDomain (waitValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiROMManDay) REFERENCES input_static_romManDaysValueDomain (romManDaysValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiResolution) REFERENCES input_static_resolutionValueDomain (resolutionValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiSquadId) REFERENCES input_dynamic_squads (squadId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiTargetVersionGroupId) REFERENCES aux_dynamic_targetVersionGroups (targetVersionGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);


