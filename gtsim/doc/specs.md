


# Task

A Task has the following properties:

* A Task is uniquely identified by a name.
* A Task belongs to a Project, identified by a Project Name.
* ManDaysDistr: A mand.days distribution. A sample from that distribution represents the effort required to complete that Task.

# Task Dependencies

There is only one kind of dependencies between Tasks.

It is a pre-requisite dependency: A Task can only start when some (0 or many) other Tasks are finished.

We assume that there is no dependency cycles.


# Resource

A Resource has the following properties:

* A Resource is uniquely identified by a name.
* Eff: An efficiency, expressed in man.days per working day. At any time T, the efficiency depends on the subset of Active Assignments which are worked on at that time T (see Assignment section below).
* NbActiveAssignmentsDistr: A distribution for the number of Active Assignments.



# Potential Resources

Each Task is associated with a set of Potential Resources. Given a Task, a Potential Resource is a Resource that can potentially be assigned to that Task.

Conversely, a Resource is associated with a set of Potential Tasks.

Potential Resources can be represented by the tuple:

```
(Task, Resource)
```
Each Task should at least have one Potential Resource.

# Assignment.

An Assignment is the fact that a Resource is assigned to a Task (and can work on that Task).

An Assignment has the following properties:

* Task: the task of the Assignment.
* Resource: the Resource of the Assignment.
* StartTime: the time the Resource has been assigned to the Task and started to (potentially) work on it.
* Mandays: the specific number of man.days required to complete the Task.
* RemainingManDays: the number of remaining man.days required to finished the Task. These remaining man.days have to be interpreted with respect to a specific point in time (see Algorithm).
* An Assignment is Active when RemainingManDays > 0.
* An Assignment is Completed when RemainingManDays = 0.
* EndTime: the time at which the Assignment has been completed. This field is only relevant when an Assignment is Completed.
* An Assignment is uniquely identified by its Task.

An Active Assignment can be represented by the tuple:

```
(Task, Resource, Mandays, RemainingManDays, StartTime)
```

An Completed Assignment can be represented by the tuple:

```
(Task, Resource, Mandays, StartTime, EndTime)
```
# Global Parameters


**WaitingAssignmentProb**: the probability that an Assignment is on hold, waiting.
That is, its assigned resource does not work on it. That accounts for blockers and impediments during development and for what is called the Flow Efficiency in a queue/flow based development process.
Moreover, we can measure that Flow Efficiency so we'll have a way to tune the algo.

# Some considerations about the development process.

* A Resource always does its best to be assigned to at least one Task.
* A Resource always does its best to actively work on all of its Assignments.
* In practice, a Resoure is not always able to work on some Assignments. This can be due to impediments, blockers, meetings or the sheer systemic inefficiency of the development process. These blockers occur randomly, at any time.
* At any time, a Resource can decide (with some probability) to start to work on a new Task. That explains why a Resource can be assigned to multiple Tasks (up to a maximum).



# Simulation Algorithm

## State

Given a set of Tasks, Task Dependencies, Resources and Potential Resources, the State of a Simulation is defined by:

* Tc: The current (simulation) time.
* ~~Tn: the next (simulation) time.~~
* ActiveAssignments : the set of Active Assignments.
* CompletedAssignments : the set of Completed Assignments.
* AvailableTasks: the set of Tasks that can be chosen by Resources, taking into account the dependencies between Tasks. This set only contains Tasks that either do not depend on any other Task or depend on completed Tasks (Tasks in CompletedAssignments).
* DependentTasks: the set of Tasks that cannot be picked up because they are depending on at least a yet un-finished Task.

## Initial State

The initial State is defined by:

* Tc = 0
* ~~Tn > Tc~~
* ActiveAssignements = empty
* CompletedAssignements = empty
* AvailableTasks = set of Tasks which do not depend on any other Task.
* DependentTasks = All original Tasks - AvailableTasks.

## Steps

### Step 1: Choose the next time, Tn.

Choose Tn > Tc.

The choice of the interval (Tn-Tc) does not really matter.

However, it should be close to the system's time constant.

In practice, a good time constant is the smallest mandays value that can be sampled from the ManDaysDistr distribution.

I.E If the smallest task effort is around a day, (Tn-Tc) could be a day.



### Step 2: Choose new Active Assignments.

For each Resource (chosen randomly, from a uniform distribution):

* From the distribution NbActiveAssignmentsDistr, sample Nca, the current number of ActiveAssignments for this Resource.
* if Nca > number of Active Assignemnts (Naa) for this resource then

  * Randomy choose as many as (Nca - Naa) Tasks from the set of Available Tasks.
  * REM: there might be less than (Nca - Naa) available tasks.
  * Remove these chosen Tasks from AvailableTasks.
  * For each Task, create new Active Assignments.

    * mandays = sample from ManDaysDistr.
    * insert `(the Task, the Resource, mandays, mandays, Tc)` into ActiveAssignments.


### Step 3: Choose Active Assignments to work on (Running Assignments).
For each Resource:

* For each of its Active Assignments, use WaitingAssignmentProb to determine whether the Assignment is on hold or not.
* Create Srunning, the subset of ActiveAssignments which are not on hold.

### Step 4: Choose and update Active Assignments.

For each Resource:

* Choose an efficiency: eff. eff depends on that Resource and on the set S (see Remarks below).
* For each ActiveAssignments in Srunning, update its RemainingManDays:

\[RemainingManDays' = RemainingManDays - eff * (Tn - Tc)\]

\[EndTime = Tc + RemainingManDays' / eff\]

* If RemainingManDays' <= 0 then

    * compute the Assignment end time (Te) by solving:

    \[0 = RemainingManDays - eff * (Te - Tc)\]

    * Remove this Assignment from ActiveAssignments.
    * Create a new Completed Assignment and add it to the set CompletedAssignments:
        ```
        (Task, Resources, Mandays, StartTime, Te)
        ```
    * Update the AvailableTasks with all the Tasks from DependentTasks which have this new Complete Assignment's Task as last and final dependency.
    * Remove the newly AvailableTasks from DependentTasks.

Now, doing so has some implications:

Suppose that at least one Assignment T1 is completed by R1 during this update, at T = Te: Tc < Te <= Tn.

In addition, suppose R1 is assigned to more than one running Assignments: I.E. T2, T3.

Then, we have the following:

* For all T: Tc < T <= Te, R1 is working on all 3 Assignments with an efficiency that corresponds to 3 parallel running Assignments. That is correct.

* For all T: Te < T <= Tc, the R1 is working on at most 2 Assignments but still with that same efficiency that corresponds to 3 parallel Assignments. And this is not correct.

So the solution is:
* After a complete update (all resources), compute the smallest time at which an Assignment has been completed, if any: Tme.
* It is also necessary run the new update on exactly the same subset of running Active Assignments. That is why Srunning is computed as a specific step.
* Re-start the update with Tn = Tme.
* Tme becomes the new value for Tn.

That way, we never go "to far" in time and we are sure that we stop as soon as a event that globally impacts the system occurs.


### Step 5: Iterate or Stop

The simulation terminates when:
* the number of completed Tasks is equal to the number of initial Tasks.

REM: the algorithm might not terminate if the preconditions on Tasks, Task Dependencies and Resources are not met.

Otherwise:

* Tc = Tn
* Iterate back from Step 1.

The most useful piece of information in the final state is the set of Completed Assignments, which is basically a Gantt Chart.


## Remarks

### Resource efficiency

In the most general case, the current Resource efficiency depends on that Resource, the chosen subset of its ActiveAssignments and the state of the simulation.

### Number of Active Assignment Distribution

To abstract even more, the new set of ActiveAssignments could be a distribution that depends on:

* The set of AvailableTasks.
* The current set of ActiveAssignments.
* The signature could be:

  ```
  ... :: Resource -> AvailableTasks -> ActiveAssignments
         -> Random (ActiveAssignments, AvailableTasks)
  ```
* Doing so will render the number of Active Assignment independent from the time step chosen. Remember: we sample at every step, so there might be a tendency to always be stuck to the maximum number of Assignment (in particular when Task man.days is large).

# Testing

## Invariants

* All key/uniqueness constraints stated above.
* all tasks in CompletedAssignments, ActiveAssignments, AvailableTasks and DependentTasks = all initial Tasks.
* For ActiveAssignments:
  * Mandays >= RemainingManDays
  * RemainingManDays > 0
  * StartTime <= Tc
* For CompletedAssignments:
    * Mandays >= 0
    * StartTime < EndTime
* Initial State: by construction
* Final State:
  * Completed Tasks = original Tasks
  * AvailableTasks = empty
  * DependentTasks = empty
  * ActiveAssignments = empty
  * Task dependencies are respected: for any two Tasks TA & TB, TB depending on TA, the start time of TB >= end time of TA.

In addition, there is also a "transition" invariant:

* Two consecutive states should not be equal.

# Questions from Andres

1. The `EndTime` of an assignment can only usefully be determined once the assignment is `Completed`. The section on assignments states that the `EndTime` is part of the representation of `Active` assignments. I think this should be removed.

Correct. Better definition added.

2. The item `Tn` is described to be part of the state of the algorithm. But it's not really. It's only chosen after the start of each step. I think either should `Tn` just be removed from the `State`, or it should be clarified that `Tn` has a helper role.

Indeed and it is strikethroughed: * ~~Tn: the next (simulation) time.~~
I forgot to delete it.

REM : I am using Atom + markdown-preview-plus plugin: it works ok and renders equations.

3. In the section on `Number of Active Assignment Distribution` it is suggested that making the distribution depending on the set of `AvailableTasks` and the current set of `ActiveAssignments` would make the number of active assignments independent from the time step chosen. That's not really true. If it's still a random distribution and randomly yields different numbers of `ActiveAssignments` for the same inputs, then it still depends on the sampling rate how quickly we choose a new task. All in all, I consider this dependency on the time unit / sampling rate the most critical point of the algorithm as currently described.

Agreed, that is my main concern here too.

Let's examine what happens in a simple case. Let's suppose:
* time steps <<< average man.days for tasks.
* NbActiveAssignmentsDistr: uniformly sampled from some range [0 .. N].

In that case, after some time, the number of Active Assignments will always be close to N.

Is that good? Well, yes and no.
1) Yes because, in practice (in the real world), if the dev process is not properly controlled/monitored, that is exactly what happens: devs tend to pick up as many tasks they can.
2) No because, once again, in practice, a high number of concurrent active assignments per resource negatively impacts the overall performance/throughput and makes the system much less stable and predictable. That is something we have to avoid.

In fact, in the simple case above, what we want to ensure is: the distribution of Active Assignments (and that depends on time) is uniform in the range [0 .. N].

More precisely, in that simple case again: for any given resource, if Ni is the number of Active Assignments at time Ti, i = 0 ... n, T0 = 0, Tn = last simulation time, then Ni is uniformly distributed in range [0 .. N].


FYI: I am monitoring the dev process, every day, and I am trying to keep these things well balanced.

Now what can we do?

IMO, the main question is: should that feature be controlled by the algo or can it be delegated to `NbActiveAssignmentsDistr`?

In the latter case, the most generic formulation for `NbActiveAssignmentsDistr` I can think of is something such as (a bit imprecise but you get the idea):

```
data NbActiveAssignmentsDistr = MkNbActiveAssignmentsDistr { unNbActiveAssignmentsDistr :: State -> Time -> Random (Int, NbActiveAssignmentsDistr) }
```

So, the choice of Nca is made with full (current and past) knowledge of the simulation.

In the former case: I still have to think about it, but it can definitively be embedded inside the formultion above.

Anyway, whatever the future solution, it will be quite easy to adapt the code as that does not impact the core of the algo.


4. It might be useful to clarify/spell out that the distribution of active assignments also effectively determines the maximum number of assignments a resource can have.

Well, not really:
1) the algo does not explicitly rely on this maximum number and can even cope with an infinite number of active assignments, that is, when Nca -> infinity.
2) It can also cope with Nca = 0. But in the case of `NbActiveAssignmentsDistr = constant 0` for any resource, the algo never terminates. That is why I added a new "transition" invariant.
3) So, the choice of NbActiveAssignmentsDistr is really something that is part of a specific setting, when we run the simulation.


5. It might be useful to clarify/spell out what the *output* of the algorithm is.

The output is the final state. Now, within the final state, the useful piece of information is the set of Completed Assignments, which is basically a gantt chart.
(added).

Once we have run a large number of simulations, we can do any kind of post-processing on the gantt charts or save them into a file or database.
But these are details for the moment.
