
-- DROP DATABASE IF EXISTS sdlc_db;
-- CREATE DATABASE sdlc_db;




-- \c sdlc_db;

CREATE TABLE priorityDomain (
  priorityVal text,

  CONSTRAINT PKC_priorityDomain PRIMARY KEY (priorityVal)
);

insert into priorityDomain values
    ('ShowStopper')
  , ('Critical')
  , ('Major')
  , ('Normal')
  , ('Minor');


CREATE TABLE stateDomain (
  stateVal text,

  CONSTRAINT PKC_stateDomain PRIMARY KEY (stateVal)
);

insert into stateDomain values
    ('Backlog')
  , ('Planning')
  , ('Selected')
  , ('InProgress')
  , ('Review')
  , ('Done')
  , ('Neutral');

CREATE TABLE waitDomain (
  waitVal text,

  CONSTRAINT PKC_waitDomain PRIMARY KEY (waitVal)
);

insert into waitDomain values
    ('Running')
  , ('Waiting');

CREATE TABLE typeDomain (
  typeVal text,

  CONSTRAINT PKC_typeDomain PRIMARY KEY (typeVal)
);

insert into typeDomain values
    ('Task')
  , ('User Story')
  , ('Bug')
  , ('TestCase')
  , ('Other');

CREATE TABLE threeDDomain (
  threeDVal text,

  CONSTRAINT PKC_threeDDomain PRIMARY KEY (threeDVal)
);

insert into threeDDomain values
    ('Design')
  , ('Development')
  , ('Documentation')
  , ('Test');

CREATE TABLE romManDaysDomain (
  romManDaysVal text,

  CONSTRAINT PKC_romManDaysDomain PRIMARY KEY (romManDaysVal)
);

insert into romManDaysDomain values
    ('Days')
  , ('Weeks')
  , ('Months')
  , ('Quarters');

CREATE TABLE resolutionDomain (
  resolutionVal text,

  CONSTRAINT PKC_resolutionDomain PRIMARY KEY (resolutionVal)
);

insert into resolutionDomain values
    ('Successful')
  , ('Aborted')
  , ('Duplicate')
  , ('Obsolete');

CREATE TABLE linkTypeDomain (
  linkType text,

  CONSTRAINT PKC_linkTypeDomain PRIMARY KEY (linkType)
);

insert into linkTypeDomain values
    ('ParentFor')
  , ('SubTaskOf')
  , ('MustStartAfter')
  , ('IsPreRequisiteFor')
  , ('DependsOn')
  , ('Duplicates')
  , ('RelatesTo')
  , ('IsDuplicatedBy')
  , ('IsRequiredFor')
  , ('TestedBy');


CREATE TABLE valueChangeTypeDomain (
  valueChangeType text,

  CONSTRAINT PKC_valueChangeTypeDomain PRIMARY KEY (valueChangeType)
);

insert into valueChangeTypeDomain values
    ('Time Updated')
  , ('Updated By')
  , ('State Changed')
  , ('Wait Changed');

CREATE TABLE developers (
  developerName text NOT NULL,
  otherDetails  text,

  CONSTRAINT PKC_developers PRIMARY KEY (developerName)
);





CREATE TABLE tickets (
  ticketId   text NOT NULL,
  ticketType text NOT NULL,

  CONSTRAINT PKC_ticketId PRIMARY KEY (ticketId),
  UNIQUE (ticketId, ticketType),
  FOREIGN KEY (ticketType) REFERENCES typeDomain (typeVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);



CREATE TABLE links (
  ticketId       text NOT NULL,
  linkType       text NOT NULL,
  linkedTicketId text NOT NULL,

  CONSTRAINT PKC_links PRIMARY KEY (ticketId, linkType, linkedTicketId),
  FOREIGN KEY (ticketId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (linkType) REFERENCES linkTypeDomain (linkType)
  ON DELETE RESTRICT ON UPDATE CASCADE
);



CREATE TABLE issueStateChanges (
  issueId     text    NOT NULL,
  updateTime  Integer NOT NULL,
  updater     text    NOT NULL,
  oldStateVal text    NOT NULL,
  newStateVal text    NOT NULL,

  CONSTRAINT PKC_issueStateChanges PRIMARY KEY (issueId, updateTime, updater, oldStateVal, newStateVal),
  FOREIGN KEY (issueId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (updater) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (oldStateVal) REFERENCES stateDomain (stateVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (newStateVal) REFERENCES stateDomain (stateVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE issueWaitChanges (
  issueId    text    NOT NULL,
  updateTime Integer NOT NULL,
  updater    text    NOT NULL,
  oldWaitVal text    NOT NULL,
  newWaitVal text    NOT NULL,

  CONSTRAINT PKC_issueWaitChanges PRIMARY KEY (issueId, updateTime, updater, oldWaitVal, newWaitVal),
  FOREIGN KEY (issueId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (updater) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (oldWaitVal) REFERENCES waitDomain (waitVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (newWaitVal) REFERENCES waitDomain (waitVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE stateTransitionDomain (
  stateTransitionVal text NOT NULL,
  CONSTRAINT PKC_stateTransition PRIMARY KEY (stateTransitionVal)
);

insert into stateTransitionDomain values
    ('Backlog')
  , ('Selected')
  , ('InProgress')
  , ('InReview')
  , ('Done')
  , ('IllegalStateTransitions');


CREATE TABLE stateTransitions (
  stateTransitionId    text    NOT NULL,
  stateTransitionVal   text    NOT NULL,
  backlogTime          timestamp,
  selectedTime         timestamp,
  progressStartTime    timestamp,
  reviewStartTime      timestamp,
  doneTime             timestamp,

  CONSTRAINT PKC_stateTransitions PRIMARY KEY (stateTransitionId),
  FOREIGN KEY (stateTransitionVal) REFERENCES stateTransitionDomain (stateTransitionVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);



CREATE TABLE ytIssueDetails (
  ytiIssueId              text    NOT NULL,
  ytiType                 text    NOT NULL,
  ytiSummary              text    NOT NULL,
  ytiDescription          text    NOT NULL,
  ytiCreated              timestamp NOT NULL,
  ytiUpdatedAt            timestamp,
  ytiProject              text    NOT NULL,
  ytiNumber               Integer NOT NULL,
  ytiState                text    NOT NULL,
  ytiWait                 text    NOT NULL,
  ytiDueDate              timestamp NOT NULL,
  ytiROMManDay            text,
  ytiSquad                text,
  ytiOwner                text,
  ytiResolution           text    NOT NULL,
  ytiBlockedDays          Integer NOT NULL,

  CONSTRAINT PKC_YtIssueDetails PRIMARY KEY (ytiIssueId),
  FOREIGN KEY (ytiIssueId, ytiType) REFERENCES tickets (ticketId, ticketType)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiType) REFERENCES typeDomain (typeVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiState) REFERENCES stateDomain (stateVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiWait) REFERENCES waitDomain (waitVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiROMManDay) REFERENCES romManDaysDomain (romManDaysVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiResolution) REFERENCES resolutionDomain (resolutionVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiIssueId) REFERENCES stateTransitions (stateTransitionId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE targetVersion (
  ytiIssueId       text NOT NULL,
  targetVersion    text    NOT NULL,

  CONSTRAINT PKC_targetVersion PRIMARY KEY (ytiIssueId, targetVersion),
  FOREIGN KEY (ytiIssueId) REFERENCES ytIssueDetails (ytiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);


CREATE TABLE ytTaskDetails (
  yttTaskId            text    NOT NULL,
  yttTaskType          text    default 'Task',
  yttSummary           text    NOT NULL,
  yttDescription       text    NOT NULL,
  yttCreated           timestamp NOT NULL,
  yttUpdatedAt         timestamp,
  yttProject           text    NOT NULL,
  yttNumber            Integer NOT NULL,
  yttState             text    NOT NULL,
  yttWait              text    NOT NULL,
  yttThreeDVal         text    NOT NULL,
  yttBlockedDays       Integer NOT NULL,
  yttParent            text    NOT NULL,

  CONSTRAINT PKC_ytTaskDetails PRIMARY KEY (yttTaskId),
  FOREIGN KEY (yttTaskId, yttTaskType) REFERENCES tickets (ticketId, ticketType),
  FOREIGN KEY (yttTaskId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttState) REFERENCES stateDomain (stateVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttWait) REFERENCES waitDomain (waitVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttTaskId) REFERENCES stateTransitions (stateTransitionId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE TaskAssignee (
  yttTaskId     text NOT NULL,
  developerName text NOT NULL,

  CONSTRAINT PKC_TaskAssignee PRIMARY KEY (yttTaskId, developerName),
  FOREIGN KEY (yttTaskId) REFERENCES ytTaskDetails (yttTaskId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

-- Tables specific for Testing Projects :-

CREATE TABLE yttpTypeDomain (
  yttpTypeVal text NOT NULL,

  CONSTRAINT PKC_yttpTypeDomain PRIMARY KEY (yttpTypeVal)
);

insert into yttpTypeDomain values
    ('Review')
  , ('Document Request')
  , ('Risk')
  , ('Action')
  , ('Issue')
  , ('Test Case')
  , ('Decision');

CREATE TABLE yttpStateDomain (
  yttpStateVal text NOT NULL,

  CONSTRAINT PKC_yttpStateDomain PRIMARY KEY (yttpStateVal)
);

insert into yttpStateDomain values
    ('Open')
  , ('Obsolete')
  , ('Verified')
  , ('Blocking')
  , ('Done')
  , ('Selected');

CREATE TABLE yttpPriorityDomain (
  yttpPriorityVal text,

  CONSTRAINT PKC_yttpPriorityDomain PRIMARY KEY (yttpPriorityVal)
);

insert into yttpPriorityDomain values
    ('ShowStopper')
  , ('High')
  , ('Major')
  , ('Medium')
  , ('Low')
  , ('Normal')
  , ('Critical')
  , ('Minor');

CREATE TABLE yttpReviewStatusDomain (
  yttpReviewStatusVal text,

  CONSTRAINT PKC_yttpReviewStatusDomain PRIMARY KEY (yttpReviewStatusVal)
);

insert into yttpReviewStatusDomain values
    ('WIP')
  , ('To be reviewed')
  , ('Review done')
  , ('Needs update')
  , ('To be deleted');

CREATE TABLE yttpAutomationStatusDomain (
  yttpAutomationStatusVal text,

  CONSTRAINT PKC_yttpAutomationStatusDomain PRIMARY KEY (yttpAutomationStatusVal)
);

insert into yttpAutomationStatusDomain values
    ('Manual')
  , ('Automated')
  , ('To be automated');

CREATE TABLE yttpTestResultDomain (
  yttpTestResultVal text,

  CONSTRAINT PKC_yttpTestResultDomain PRIMARY KEY (yttpTestResultVal)
);

insert into yttpTestResultDomain values
    ('Passed')
  , ('Failed');


CREATE TABLE yttpTargetOSDomain (
  yttpTargetOSVal text,

  CONSTRAINT PKC_yttpTargetOSVal PRIMARY KEY (yttpTargetOSVal)
);

insert into yttpTargetOSDomain values
    ('Linux')
  , ('Windows')
  , ('All Platforms')
  , ('MacOS')
  , ('AnySingle');

CREATE TABLE yttpTestingTypeDomain (
  yttpTestingTypeVal text,

  CONSTRAINT PKC_yttpTestingTypeDomain PRIMARY KEY (yttpTestingTypeVal)
);

insert into yttpTestingTypeDomain values
    ('Integration Test')
  , ('UI Test')
  , ('E2E Test')
  , ('API Test')
  , ('Component Test')
  , ('UX Test')
  , ('NFT')
  , ('Unit Test');



CREATE TABLE yttpBrowserVersionDomain (
  yttpBrowserVersionVal text,

  CONSTRAINT PKC_yttpBrowserVersionDomain PRIMARY KEY (yttpBrowserVersionVal)
);

insert into yttpBrowserVersionDomain values
    ('Google Chrome 68')
  , ('Opera 55')
  , ('Google Chrome 69');


/*
This is the table for all "test" projects , right

Comments:
* use date/datetime (UTC-0) for all time attributes
* use DT (Daedalus Testing ) as an example for for the fields.
* I.E yttpiTestResult is not used , replaced by Passed in Versions and Failed in Versions
* So make sure the db corresponds to DT https://iohk.myjetbrains.com/youtrack/issues/DT .
Anatoli will adapt the other test projects in YT so that all projects conform to DT.

For the future: capture history of Passed in Versions, Failed in Versions and Blocked in Versions.
*/

CREATE TABLE yttpIssueDetails (
  yttpiIssueId           text    NOT NULL,
  yttpiType              text    NOT NULL,
  yttpiSummary           text    NOT NULL,
  yttpiDescription       text    NOT NULL,
  yttpiCreated           timestamp NOT NULL,
  yttpiUpdatedAt         timestamp,
  yttpiProject           text    NOT NULL,
  yttpiNumber            Integer NOT NULL,
  yttpiState             text    NOT NULL,
  yttpiPriority          text    NOT NULL,
  yttpiReviewStatus      text    NOT NULL,
  yttpiAutomationStatus  text    NOT NULL,
  yttpiInRegressionSuite Bool    NOT NULL,
  yttpiInSmokeTest       Bool    NOT NULL,
  yttpiExecutiontime     Integer,
  yttpiTestResult        text,

  CONSTRAINT PKC_yttpIssueDetails PRIMARY KEY (yttpiIssueId),
  FOREIGN KEY (yttpiType) REFERENCES yttpTypeDomain (yttpTypeVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpiState) REFERENCES yttpStateDomain (yttpStateVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpiPriority) REFERENCES yttpPriorityDomain (yttpPriorityVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpiReviewStatus) REFERENCES yttpReviewStatusDomain (yttpReviewStatusVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpiAutomationStatus) REFERENCES yttpAutomationStatusDomain (yttpAutomationStatusVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpiTestResult) REFERENCES yttpTestResultDomain (yttpTestResultVal)
  ON DELETE RESTRICT ON UPDATE CASCADE

);

CREATE TABLE yttpTargetOS (
  yttpiIssueId  text NOT NULL,
  yttpTargetOS text NOT NULL,

  CONSTRAINT PKC_yttpTargetOS PRIMARY KEY (yttpiIssueId, yttpTargetOS),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpTargetOS) REFERENCES yttpTargetOSDomain (yttpTargetOSVal)
  ON DELETE RESTRICT ON UPDATE CASCADE

);


CREATE TABLE yttpTestingType (
  yttpiIssueId  text NOT NULL,
  yttpTestingType text NOT NULL,

  CONSTRAINT PKC_yttpTestingType PRIMARY KEY (yttpiIssueId, yttpTestingType),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpTestingType) REFERENCES yttpTestingTypeDomain (yttpTestingTypeVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);


CREATE TABLE yttpAssignee (
  yttpiIssueId  text NOT NULL,
  developerName text NOT NULL,

  CONSTRAINT PKC_yttpAssignee PRIMARY KEY (yttpiIssueId, developerName),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (developerName) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);


CREATE TABLE yttpPassedVersions (
  yttpiIssueId  text NOT NULL,
  yttpVersion text NOT NULL,

  CONSTRAINT PKC_yttpPassedVersions PRIMARY KEY (yttpiIssueId, yttpVersion),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE yttpFailedVersions (
  yttpiIssueId  text NOT NULL,
  yttpVersion text NOT NULL,

  CONSTRAINT PKC_yttpFailedVersions PRIMARY KEY (yttpiIssueId, yttpVersion),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE yttpBlockedVersions (
  yttpiIssueId  text NOT NULL,
  yttpVersion text NOT NULL,

  CONSTRAINT PKC_yttpBlockedVersions PRIMARY KEY (yttpiIssueId, yttpVersion),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE yttpCoveredComponents (
  yttpiIssueId  text NOT NULL,
  yttpComponent text NOT NULL,

  CONSTRAINT PKC_yttpCoveredComponents PRIMARY KEY (yttpiIssueId, yttpComponent),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE yttpBrowserAndVersions (
  yttpiIssueId  text NOT NULL,
  yttpBrowserVersionVal text NOT NULL,

  CONSTRAINT PKC_yttpBrowserAndVersions PRIMARY KEY (yttpiIssueId, yttpBrowserVersionVal),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttpBrowserVersionVal) REFERENCES yttpBrowserVersionDomain (yttpBrowserVersionVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE yttpLinks (
  yttpiIssueId  text NOT NULL,
  yttpLinkedTicketId   text NOT NULL,
  yttpLinkedObjectType text NOT NULL,
  yttpLinkRole         text NOT NULL,

  CONSTRAINT PKC_yttpLinks PRIMARY KEY (yttpiIssueId, yttpLinkedTicketId),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

