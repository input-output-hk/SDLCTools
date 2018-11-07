
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

CREATE TABLE iohksStateDomain (
  iohksStateVal text,

  CONSTRAINT PKC_iohksStateDomain PRIMARY KEY (iohksStateVal)
);

insert into iohksStateDomain values
    ('IohksSubmitted')
  , ('IohksReadyToSolve')
  , ('IohksFixed')
  , ('IohksDone');

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
  , ('Done');

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
    ('TaskType')
  , ('IssueType')
  , ('TestCase')
  , ('OtherType');

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
  , ('IsRequiredFor');

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

/*
squadId: we use text to identify squads
dk : fixed ^^
squadSize : duplicate information: we can get it from squadDetails
dk : fixed ^^

What to do in case of a 1 person squad:
squadLead should be there and then we have to decide whether or not adding him as a squad member.
It a subjective choice.
But if we do, then we have to ensure, via a constraint, that the squad lead is a squad member as well.

dk : yes, indeed we have to check this constraint but for now I'm leaving it considering this can be tested
     with a ON INSERT trigger or sth else later.
*/

CREATE TABLE squads (
  squadId   text NOT NULL,
  squadLead text NOT NULL,

  CONSTRAINT PKC_squads PRIMARY KEY (squadId),
  FOREIGN KEY (squadLead) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE squadDetails (
  squadId     text NOT NULL,
  squadMember text NOT NULL,

  CONSTRAINT PKC_squadDetails PRIMARY KEY (squadId,squadMember),
  FOREIGN KEY (squadId) REFERENCES squads (squadId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (squadMember) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

/*
There is a alternative model here, which is more RM in my opinion.
dk : fixed ^^
 */


/*
The associate predicate is then (see the book: Database in Depth)

The developer {developerName} is/was assigned to task identified by {yttTaskId}
dk : didn't get it (btw I haven't bought the book yet) ^^ .

Generic type is a temprorary data structure used in the code, not sure it should be in the DB.
dk : yes its not necessary, I created this table with the view that this can be used to have a
     collective store for all valid YT tickets. well removed for now.

target versions is a multi-valued field in YT.
This is non-sense and should be modified in YT.
But you could not really know
So, for the sake of simplicity, we can make it a single-valued field.
dk : seems okay for now.
*/

CREATE TABLE targetVersionDomain (
  targetVersion text NOT NULL,

  CONSTRAINT PKC_targetVersionDomain PRIMARY KEY (targetVersion)
);

CREATE TABLE aux_targetVersionGroups (
  targetVersionGroupId Integer NOT NULL,

  CONSTRAINT PKC_targetVersionGroups PRIMARY KEY (targetVersionGroupId)
);

CREATE TABLE targetVersionGroupDetails (
  targetVersionGroupId Integer NOT NULL,
  targetVersion        text    NOT NULL,

  CONSTRAINT PKC_targetVersionGroupDetails PRIMARY KEY (targetVersionGroupId, targetVersion),
  FOREIGN KEY (targetVersionGroupId) REFERENCES aux_targetVersionGroups (targetVersionGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (targetVersion) REFERENCES targetVersionDomain (targetVersion)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE tickets (
  ticketId   text NOT NULL,
  ticketType text NOT NULL,

  CONSTRAINT PKC_ticketId PRIMARY KEY (ticketId),
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


/*
1) Here also : more idiomatic solution which does not use a 'group'. Groups are ok if they  have a) an independent
existence or b) we need to express FKs between table and views (sth SQL does not accept).
dk : fixed ^^

2) the Changes tables mixes 2 concepts: changes of State and change of Wait. The general rule is that
a table only captures 1 and only 1 concept and a concept is captured by just 1 and only 1 table.
dk : fixed ^^

A more idiomatic solution could be :

table IssueStateChange
(
  issueId   -- FK to issue table
  updateTime
  updater       -- FK to dev
  oldStateVal text,  -- FK to stateDomain table etc
  newStateVal text
)

The predicate is:

At time {updateTime}, the value of the state field of the issue {issueId} has transitioned from
{oldStateVal} to {newStateVal} and the change was done by {updater}
*/

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
    ('STBacklog')
  , ('STSelected')
  , ('STInProgress')
  , ('STInReview')
  , ('STDone')
  , ('STIllegalStateTransitions');

/*

data StateTransitions =
    STBacklog Int
  | STSelected Int Int
  | STInProgress Int Int Int
  | STInReview Int Int Int Int
  | STDone Int Int Int Int Int
  | STIllegalStateTransitions

  This data type is hard to model in RM.

  I think you have found the right way to do it.

But we have to clear about the predicate as the meaning of the ***Time attributes depends
on the value of stateTransition

BTW: in this case, it makes sense to have a reference from  issue/task to stateTransitions
Thus ok for the surrogate key stateTransitionId
*/

CREATE TABLE stateTransitions (
  stateTransitionId    Integer NOT NULL,
  ticketId             text    NOT NULL,
  stateTransitionVal text    NOT NULL,
  backlogTime          Integer,
  selectedTime         Integer,
  progressStartTime    Integer,
  reviewStartTime      Integer,
  doneTime             Integer,

  CONSTRAINT PKC_stateTransitions PRIMARY KEY (stateTransitionId),
  FOREIGN KEY (ticketId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (stateTransitionVal) REFERENCES stateTransitionDomain (stateTransitionVal)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

/*
Not sure we have to report errors in the DB.
If there are errors, the importer (code) can report them on the screen.
No need to complexify the DB here
Or just use a simple text field to store the error.

dk : ok fine, removed errors.

And another question : if an error occurs for an issue, then we simply do not have any info about it.
So if we have to store errors, it should be at the level of the super entiry (Ticket, see above).
dk : no errors no problems.

Optional: We might want to drop priority fields: no one uses them.
dk : ok fixed
*/



CREATE TABLE ytIssueDetails (
  ytiIssueId              text    NOT NULL,
  ytiType                 text    NOT NULL,
  ytiSummary              text    NOT NULL,
  ytiDescription          text    NOT NULL,
  ytiCreated              Integer NOT NULL,
  ytiUpdatedAt            Integer,
  ytiProject              text    NOT NULL,
  ytiNumber               Integer NOT NULL,
  ytiState                text    NOT NULL,
  ytiWait                 text    NOT NULL,
  ytiDueDate              Integer NOT NULL,
  ytiROMManDay            text,
  ytiSquadId              text,
  ytiTargetVersionGroupId Integer,
  ytiOwner                text,
  ytiResolution           text    NOT NULL,
  ytiStateTransitionId    Integer NOT NULL,
  ytiBlockedDays          Integer NOT NULL,

  CONSTRAINT PKC_YtIssueDetails PRIMARY KEY (ytiIssueId),
  FOREIGN KEY (ytiIssueId) REFERENCES tickets (ticketId)
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
  FOREIGN KEY (ytiSquadId) REFERENCES squads (squadId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiTargetVersionGroupId) REFERENCES aux_targetVersionGroups (targetVersionGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiStateTransitionId) REFERENCES stateTransitions (stateTransitionId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE ytTaskDetails (
  yttTaskId            text    NOT NULL,
  yttSummary           text    NOT NULL,
  yttDescription       text    NOT NULL,
  yttCreated           Integer NOT NULL,
  yttUpdatedAt         Integer,
  yttProject           text    NOT NULL,
  yttNumber            Integer NOT NULL,
  yttState             text    NOT NULL,
  yttWait              text    NOT NULL,
  yttThreeDVal       text    NOT NULL,
  yttStateTransitionId Integer NOT NULL,
  yttBlockedDays       Integer NOT NULL,
  yttParent            text    NOT NULL,

  CONSTRAINT PKC_ytTaskDetails PRIMARY KEY (yttTaskId),
  FOREIGN KEY (yttTaskId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttState) REFERENCES stateDomain (stateVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttWait) REFERENCES waitDomain (waitVal)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttStateTransitionId) REFERENCES stateTransitions (stateTransitionId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE TaskAssignee (
  yttTaskId     text NOT NULL,
  developerName text NOT NULL,

  CONSTRAINT PKC_TaskAssignee PRIMARY KEY (yttTaskId, developerName),
  FOREIGN KEY (yttTaskId) REFERENCES ytTaskDetails (yttTaskId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (developerName) REFERENCES developers (developerName)
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
  , ('NFT');

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
--  yttpiTargetOS          text,
--  yttpiTestingType       text,

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
--  FOREIGN KEY (yttpiTargetOS) REFERENCES yttpTargetOSDomain (yttpTargetOSVal)
--  ON DELETE RESTRICT ON UPDATE CASCADE,
--  FOREIGN KEY (yttpiTestingType) REFERENCES yttpTestingTypeDomain (yttpTestingTypeVal)
--  ON DELETE RESTRICT ON UPDATE CASCADE

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
  yttpLinkedTicketId text NOT NULL,

  CONSTRAINT PKC_yttpLinks PRIMARY KEY (yttpiIssueId, yttpLinkedTicketId),
  FOREIGN KEY (yttpiIssueId) REFERENCES yttpIssueDetails (yttpiIssueId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

