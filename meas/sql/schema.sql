
DROP DATABASE IF EXISTS sdlc_db;
CREATE DATABASE sdlc_db;

\c sdlc_db;

CREATE TABLE priorityValueDomain (
  priorityValue text,

  CONSTRAINT PKC_priorityValueDomain PRIMARY KEY (priorityValue)
);

insert into priorityValueDomain values
    ('ShowStopper')
  , ('Critical')
  , ('Major')
  , ('Normal')
  , ('Minor');

CREATE TABLE iohksStateValueDomain (
  iohksStateValue text,

  CONSTRAINT PKC_iohksStateValueDomain PRIMARY KEY (iohksStateValue)
);

insert into iohksStateValueDomain values
    ('IohksSubmitted')
  , ('IohksReadyToSolve')
  , ('IohksFixed')
  , ('IohksDone');

CREATE TABLE stateValueDomain (
  stateValue text,

  CONSTRAINT PKC_stateValueDomain PRIMARY KEY (stateValue)
);

insert into stateValueDomain values
    ('Backlog')
  , ('Planning')
  , ('Selected')
  , ('InProgress')
  , ('Review')
  , ('Done');

CREATE TABLE waitValueDomain (
  waitValue text,

  CONSTRAINT PKC_waitValueDomain PRIMARY KEY (waitValue)
);

insert into waitValueDomain values
    ('Running')
  , ('Waiting');

CREATE TABLE typeValueDomain (
  typeValue text,

  CONSTRAINT PKC_typeValueDomain PRIMARY KEY (typeValue)
);

insert into typeValueDomain values
    ('TaskType')
  , ('IssueType')
  , ('OtherType');

CREATE TABLE threeDValueDomain (
  threeDValue text,

  CONSTRAINT PKC_threeDValueDomain PRIMARY KEY (threeDValue)
);

insert into threeDValueDomain values
    ('Design')
  , ('Development')
  , ('Documentation')
  , ('Test');

CREATE TABLE romManDaysValueDomain (
  romManDaysValue text,

  CONSTRAINT PKC_romManDaysValueDomain PRIMARY KEY (romManDaysValue)
);

insert into romManDaysValueDomain values
    ('Days')
  , ('Weeks')
  , ('Months')
  , ('Quarters');

CREATE TABLE resolutionValueDomain (
  resolutionValue text,

  CONSTRAINT PKC_resolutionValueDomain PRIMARY KEY (resolutionValue)
);

insert into resolutionValueDomain values
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
  FOREIGN KEY (ticketType) REFERENCES typeValueDomain (typeValue)
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
  oldStateVal text,  -- FK to stateValueDomain table etc
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
  FOREIGN KEY (oldStateVal) REFERENCES stateValueDomain (stateValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (newStateVal) REFERENCES stateValueDomain (stateValue)
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
  FOREIGN KEY (oldWaitVal) REFERENCES waitValueDomain (waitValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (newWaitVal) REFERENCES waitValueDomain (waitValue)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE stateTransitionValueDomain (
  stateTransitionValue text NOT NULL,
  CONSTRAINT PKC_stateTransitionValue PRIMARY KEY (stateTransitionValue)
);

insert into stateTransitionValueDomain values
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
on the value of stateTransitionValue

BTW: in this case, it makes sense to have a reference from  issue/task to stateTransitions
Thus ok for the surrogate key stateTransitionId
*/

CREATE TABLE stateTransitions (
  stateTransitionId    Integer NOT NULL,
  ticketId             text    NOT NULL,
  stateTransitionValue text    NOT NULL,
  backlogTime          Integer,
  selectedTime         Integer,
  progressStartTime    Integer,
  reviewStartTime      Integer,
  doneTime             Integer,

  CONSTRAINT PKC_stateTransitions PRIMARY KEY (stateTransitionId),
  FOREIGN KEY (ticketId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (stateTransitionValue) REFERENCES stateTransitionValueDomain (stateTransitionValue)
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
  FOREIGN KEY (ytiType) REFERENCES typeValueDomain (typeValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiState) REFERENCES stateValueDomain (stateValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiWait) REFERENCES waitValueDomain (waitValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiROMManDay) REFERENCES romManDaysValueDomain (romManDaysValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiResolution) REFERENCES resolutionValueDomain (resolutionValue)
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
  yttThreeDValue       text    NOT NULL,
  yttStateTransitionId Integer NOT NULL,
  yttBlockedDays       Integer NOT NULL,
  yttParent            text    NOT NULL,

  CONSTRAINT PKC_ytTaskDetails PRIMARY KEY (yttTaskId),
  FOREIGN KEY (yttTaskId) REFERENCES tickets (ticketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttState) REFERENCES stateValueDomain (stateValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttWait) REFERENCES waitValueDomain (waitValue)
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

/*
Here too, there is a more idiomatic way to model this.
dk : fixed ^^

*/