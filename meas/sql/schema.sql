

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

CREATE TABLE input_dynamic_YtIssue (
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
  ytiSquad   text,
  ytiowner   text,
  ytiResolution text NOT NULL,
  ytiStateTransitionId Integer NOT NULL,
  ytiBlockedDays Integer NOT NULL,
  CONSTRAINT PKC_input_dynamic_YtIssue PRIMARY KEY (ytiIssueId)
);
  
  
  



select * from input_static_priorityValueDomain;
select * from input_static_iohksStateValueDomain;
select * from input_static_stateValueDomain;
select * from input_static_waitValueDomain;
select * from input_static_typeValueDomain;
select * from input_static_threeDValueDomain;
select * from input_static_romManDaysValueDomain;
select * from input_static_resolutionValueDomain;
select * from input_static_linkTypeDomain;

