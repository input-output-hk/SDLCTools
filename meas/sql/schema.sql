

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
  otherDetails text,
  CONSTRAINT PKC_developers PRIMARY KEY (developerName)
);

CREATE TABLE squads (
  squadId Integer NOT NULL,
  squadLead text NOT NULL,
  squadSize Integer NOT NULL,
  CONSTRAINT PKC_squads PRIMARY KEY (squadId),
  CONSTRAINT CHK_nonNegative CHECK  (squadId > 0 AND squadSize > 0),
  FOREIGN KEY (squadLead) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE squadDetails (
  squadId Integer NOT NULL,
  squadMember text NOT NULL,
  CONSTRAINT PKC_squadDetails PRIMARY KEY (squadId,squadMember),
  CONSTRAINT CHK_nonNegative CHECK  (squadId > 0),
  FOREIGN KEY (squadId) REFERENCES squads (squadId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (squadMember) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE assigneeGroups (
  assigneeGroupId Integer NOT NULL,
  CONSTRAINT PKC_assigneeGroups PRIMARY KEY (assigneeGroupId)
);

CREATE TABLE assigneeGroupDetails (
  assigneeGroupId Integer NOT NULL,
  assignee text NOT NULL,
  CONSTRAINT PKC_assigneeGroupDetails PRIMARY KEY (assigneeGroupId, assignee),
  CONSTRAINT CHK_nonNegative CHECK  (assigneeGroupId >= 0),
  FOREIGN KEY (assigneeGroupId) REFERENCES assigneeGroups (assigneeGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (assignee) REFERENCES developers (developerName)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE aux_ytGenericTickets (
  ytgTicketId text NOT NULL,
  typeValue text NOT NULL,
  CONSTRAINT PKC_aux_ytgTicketId PRIMARY KEY (ytgTicketId)
);

CREATE TABLE targetVersionDomain (
  targetVersion text NOT NULL,
  CONSTRAINT PKC_targetVersionDomain PRIMARY KEY (targetVersion)
);

CREATE TABLE aux_targetVersionGroups (
  targetVersionGroupId Integer NOT NULL,
  CONSTRAINT PKC_targetVersionGroups PRIMARY KEY (targetVersionGroupId),
  CONSTRAINT CHK_nonNegative CHECK (targetVersionGroupId >= 0)
);

CREATE TABLE targetVersionGroupDetails (
  targetVersionGroupId Integer NOT NULL,
  targetVersion text NOT NULL,
  CONSTRAINT PKC_targetVersionGroupDetails PRIMARY KEY (targetVersionGroupId, targetVersion),
  CONSTRAINT CHK_nonNegative CHECK  (targetVersionGroupId >= 0),
  FOREIGN KEY (targetVersionGroupId) REFERENCES aux_targetVersionGroups (targetVersionGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (targetVersion) REFERENCES targetVersionDomain (targetVersion)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE aux_linkGroups (
  linkGroupId Integer NOT NULL,
  CONSTRAINT PKC_aux_linkGroups PRIMARY KEY (linkGroupId),
  CONSTRAINT CHK_nonNegative CHECK  (linkGroupId >= 0)
);

CREATE TABLE linkGroupDetails (
  linkGroupId Integer NOT NULL,
  linkType text NOT NULL,
  linkedTicketId text NOT NULL,
  CONSTRAINT PKC_linkGroupDetails PRIMARY KEY (linkGroupId, linkType, linkedTicketId),
  CONSTRAINT CHK_nonNegative CHECK  (linkGroupId >= 0),
  FOREIGN KEY (linkGroupId) REFERENCES aux_linkGroups (linkGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (linkType) REFERENCES linkTypeDomain (linkType)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE aux_changeGroups (
  changeGroupId Integer NOT NULL,
  CONSTRAINT PKC_aux_changeGroups PRIMARY KEY (changeGroupId),
  CONSTRAINT CHK_nonNegative CHECK  (changeGroupId >= 0)
);

CREATE TABLE Changes (
  changeId Integer NOT NULL,
  changeGroupId Integer NOT NULL,
  updateTime Integer NOT NULL,
  updater text,
  oldStateVal text,
  newStateVal text,
  oldWaitVal text,
  newWaitVal text,
  CONSTRAINT PKC_changeId PRIMARY KEY (changeId),
  CONSTRAINT CHK_nonNegative CHECK  (changeId >= 0 AND changeGroupId >= 0 AND updateTime >= 0),
  FOREIGN KEY (changeGroupId) REFERENCES aux_changeGroups (changeGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (oldStateVal) REFERENCES stateValueDomain (stateValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (newStateVal) REFERENCES stateValueDomain (stateValue)
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


CREATE TABLE stateTransitions (
  stateTransitionId Integer NOT NULL,
  ytgTicketId text NOT NULL,
  stateTransitionValue text NOT NULL,
  backlogTime Integer,
  selectedTime Integer,
  progressStartTime Integer,
  reviewStartTime Integer,
  doneTime Integer,
  CONSTRAINT PKC_stateTransitions PRIMARY KEY (stateTransitionId),
  CONSTRAINT CHK_nonNegative CHECK (stateTransitionId >= 0 AND backlogTime >= 0 AND
  selectedTime >= 0 AND progressStartTime >= 0 AND reviewStartTime >= 0 AND doneTime >= 0),
  FOREIGN KEY (ytgTicketId) REFERENCES aux_ytGenericTickets (ytgTicketId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (stateTransitionValue) REFERENCES stateTransitionValueDomain (stateTransitionValue)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE ytErrorGroups (
  ytErrorGroupId Integer NOT NULL,
  CONSTRAINT PKC_ytgErrorGroups PRIMARY KEY (ytErrorGroupId),
  CONSTRAINT CHK_nonNegative CHECK (ytErrorGroupId >= 0)
);

CREATE TABLE ytErrorDetails (
  errorId Integer NOT NULL,
  ytErrorGroupId Integer NOT NULL,
  ytError text NOT NULL,
  CONSTRAINT PKC_ytErrorDetails PRIMARY KEY (errorId),
  CONSTRAINT CHK_nonNegative CHECK (errorId >= 0 AND ytErrorGroupId >= 0 ),
  FOREIGN KEY (ytErrorGroupId) REFERENCES ytErrorGroups (ytErrorGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);

CREATE TABLE ytIssueDetails (
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
  ytiOwner   text,
  ytiResolution text NOT NULL,
  ytiLinkGroupId Integer,
  ytiChangeGroupId Integer,
  ytiStateTransitionId Integer NOT NULL,
  ytiBlockedDays Integer NOT NULL,
  ytiErrorGroupId Integer NOT NULL,
  CONSTRAINT PKC_YtIssueDetails PRIMARY KEY (ytiIssueId),
  CONSTRAINT CHK_nonNegative CHECK (ytiCreated >= 0 AND ytiUpdatedAt >= 0
  AND ytiNumber >= 0 AND ytiDueDate >= 0 AND ytiPriority1 >= 0 AND ytiPriority2 >= 0
  AND ytiPriority3 >= 0 AND ytiSquadId >=0 AND ytiTargetVersionGroupId >=0
  AND ytiLinkGroupId >=0 AND ytiChangeGroupId >=0 AND ytiStateTransitionId >=0
  AND ytiBlockedDays >=0 AND ytiErrorGroupId >=0),
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
  FOREIGN KEY (ytiLinkGroupId) REFERENCES aux_linkGroups (linkGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiChangeGroupId) REFERENCES aux_changeGroups (changeGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiStateTransitionId) REFERENCES stateTransitions (stateTransitionId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (ytiErrorGroupId) REFERENCES ytErrorGroups (ytErrorGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);


CREATE TABLE ytTaskDetails (
  yttTaskId text NOT NULL,
  yttSummary text NOT NULL,
  yttDescription text NOT NULL,
  yttCreated Integer NOT NULL,
  yttUpdatedAt Integer,
  yttProject text NOT NULL,
  yttNumber  Integer NOT NULL,
  yttState   text NOT NULL,
  yttWait    text NOT NULL,
  yttThreeDValue text NOT NULL,
  yttAssigneeGroupId Integer,
  yttLinkGroupId Integer,
  yttChangeGroupId Integer,
  yttStateTransitionId Integer NOT NULL,
  yttBlockedDays Integer NOT NULL,
  yttParent text NOT NULL,
  yttErrorGroupId Integer NOT NULL,
  CONSTRAINT PKC_YtTaskDetails PRIMARY KEY (yttTaskId),
  CONSTRAINT CHK_nonNegative CHECK (yttCreated >= 0 AND yttUpdatedAt >= 0
  AND yttNumber >= 0 AND yttAssigneeGroupId >= 0 AND yttLinkGroupId >= 0
  AND yttChangeGroupId >=0 AND yttStateTransitionId >=0 AND yttBlockedDays >=0
  AND yttErrorGroupId >=0),
  FOREIGN KEY (yttState) REFERENCES stateValueDomain (stateValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttWait) REFERENCES waitValueDomain (waitValue)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttAssigneeGroupId) REFERENCES assigneeGroups (assigneeGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttLinkGroupId) REFERENCES aux_linkGroups (linkGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttChangeGroupId) REFERENCES aux_changeGroups (changeGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttStateTransitionId) REFERENCES stateTransitions (stateTransitionId)
  ON DELETE RESTRICT ON UPDATE CASCADE,
  FOREIGN KEY (yttErrorGroupId) REFERENCES ytErrorGroups (ytErrorGroupId)
  ON DELETE RESTRICT ON UPDATE CASCADE
);
