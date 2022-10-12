(val ActiveSimulation nil)
(class Simulation
    [subclass-of Object]
    [ivars now eventQueue]
    (method time-now () now)
    (method startUp ()
        (set now 0)
        (set eventQueue (PriorityQueue new))
        ((ActiveSimulation isNil) ifFalse:
             {(self error: 'multiple-simulations-active-at-once)})
        (set ActiveSimulation self)
        self)
    (method finishUp ()
        (set ActiveSimulation nil)
        self)
    (method proceed () [locals event]
       (set event (eventQueue removeMin))
       (set now (event key))
       ((event value) takeAction))
    (method runUntil: (timelimit)
        (self startUp)
        ({(((eventQueue isEmpty) not) & (now <= timelimit))} whileTrue:
           {(self proceed)})
        (self finishUp)
        self)
    (method enter: (anObject) nil)
    (method exit:  (anObject) nil)
    (method scheduleEvent:at: (anEvent aTime)
        (eventQueue at:put: aTime anEvent))
    (method scheduleEvent:after: (anEvent aTimeInterval)
        (self scheduleEvent:at: anEvent (now + aTimeInterval)))
    (method scheduleRecurringEvents:using: (eventFactory timeStream)
        ((RecurringEvents new:atNextTimeFrom: eventFactory timeStream)
         scheduleNextEvent))
)
(class RecurringEvents [subclass-of Object]
    ; represents a stream of recurring events, each created from
    ; 'factory' and occurring at 'times'
    [ivars factory times]
    (method scheduleNextEvent ()
        (ActiveSimulation scheduleEvent:after: self (times next)))
    (method takeAction ()
        (factory new)
        (self scheduleNextEvent))
    (class-method new:atNextTimeFrom: (eventFactory timeStream)
        ((super new) init:with: eventFactory timeStream))
    (method init:with: (f s) ; private
        (set factory f)
        (set times s)
        self)
)
(class Lab
    [subclass-of Object]
    [ivars robot1free robot2free]
    (class-method new () ((super new) initLab))
    (method initLab () ; private
        (set robot1free true)
        (set robot2free true)
        self)
    (method hasARobot? () (robot1free | robot2free))
    (method takeARobot ()
         (robot1free ifTrue:ifFalse:
              {(set robot1free false) 1}
              {(set robot2free false) 2}))
    (method releaseRobot: (t)
         ((t = 1) ifTrue:ifFalse:
                  {(set robot1free true)}
                  {(set robot2free true)}))
)
(class RobotLabSimulation
    [subclass-of Simulation]
    [ivars 
     time-limit          ; time limit for using one robot
     lab                 ; current state of the lab
     robot-queue         ; the line of students waiting for a robot
     students-entered    ; the number of students who have entered the lab
     students-exited     ; the number of students who have finished and left
     timeWaiting         ; total time spent waiting in line by students
                         ; who have finished
     student-factory     ; class used to create a new student when one enters
     interarrival-times  ; stream of times between student entries
    ]
    (class-method withLimit:student:arrivals: (t s as) 
        ((super new) init-t:s:as: t s as))
    (method init-t:s:as: (t s as) ; private method
        (set time-limit         t)
        (set student-factory    s)
        (set interarrival-times as)
        self)
    (method startUp ()
        (set lab              (Lab new))
        (set students-entered 0)
        (set students-exited  0)
        (set timeWaiting      0)
        (set robot-queue      (Queue new))
        (super startUp)
        (self scheduleRecurringEvents:using: student-factory interarrival-times)
        self)
    (class-method new () (self error: 'robot-lab-simulation-needs-arguments))
    (method finishUp ()
        ('Num-finished= print) (students-exited print)
        (self printcomma)
        ('left-waiting= print) ((robot-queue size) print)
        (self printcomma)
        ('total-time-waiting= print) (timeWaiting print)
        (self printcomma)
        ('average-wait= print) ((timeWaiting div: students-exited) println)
        (super finishUp))
    (method printcomma () ; private
        (', print) (space print))
    (method enter: (aStudent)
        (set students-entered (1 + students-entered)))
    (method exit: (aStudent)
        (set students-exited  (1 + students-exited))
        (set timeWaiting      (timeWaiting + (aStudent timeWaiting))))
    (method requestRobotFor: (aStudent)
         ((lab hasARobot?) ifTrue:ifFalse:
              {(aStudent beGrantedRobot: (lab takeARobot))}
              {(robot-queue addLast: aStudent)}))

    (method releaseRobot: (aRobot)
        (lab releaseRobot: aRobot)
        ((robot-queue isEmpty) ifFalse:
           {((robot-queue removeFirst) beGrantedRobot: (lab takeARobot))}))
    (method time-limit       () time-limit)
    (method students-entered () students-entered)
)
(class Queue
    [subclass-of List]
)
(class Student
    [subclass-of Object]
    [ivars number          ; uniquely identifies this student
     status          ; 'awaiting-robot, 'finished, or a robot number
     timeNeeded      ; total work time this student needs
     timeStillNeeded ; time remaining for this student
     entryTime       ; time at which this student enters the simulation
     exitTime        ; time at which this student exits the simulation
    ]
    (method print () ('<Student print) (space print) (number print) ('> print))
    (method timeWaiting ()
        (exitTime - (entryTime + timeNeeded)))
    (method timeNeeded () (self subclassResponsibility))
    (class-method new () ((super new) init))
    (method init () ; private
      (set number          (1 + (ActiveSimulation students-entered)))
      (set status          'awaiting-robot)
      (set timeNeeded      (self timeNeeded))
      (set timeStillNeeded timeNeeded)
      (set entryTime       (ActiveSimulation time-now))
      (ActiveSimulation enter: self)
      (ActiveSimulation requestRobotFor: self)
      self)
    (method takeAction ()
       ((status = 'awaiting-robot) ifTrue:ifFalse:
          {(ActiveSimulation requestRobotFor: self)}
          {(self relinquishRobot)}))
    (method relinquishRobot ()
         (ActiveSimulation releaseRobot: status)
         ((self needsRobot?) ifTrue:ifFalse:
              {(set status 'awaiting-robot)
               (ActiveSimulation requestRobotFor: self)}
              {(set status   'finished)
               (set exitTime  (ActiveSimulation time-now))
               (ActiveSimulation exit: self)}))
    (method needsRobot? () (timeStillNeeded > 0))
    (method beGrantedRobot: (aRobot) [locals time-to-use]
         (set time-to-use (timeStillNeeded min: (ActiveSimulation time-limit)))
         (set timeStillNeeded (timeStillNeeded - time-to-use))
         (set status aRobot)
         (ActiveSimulation scheduleEvent:after: self time-to-use))
)
(class Student120 [subclass-of Student]
           ; a student needing 120 minutes of robot time
    (method timeNeeded () 120)
)
(class TwentyAtZero [subclass-of Object] ; Twenty arrivals at time zero
    [ivars num-arrived]
    (class-method new () ((super new) init))
    (method init () (set num-arrived 0) self)
    (method next ()
         ((num-arrived = 20) ifTrue:ifFalse:
             {99999}
             {(set num-arrived (1 + num-arrived))
              0}))
)
(val last-student-needed 30) ; time needed by last created AlternatingStudent
(class AlternatingStudent
    [subclass-of Student]
    (method timeNeeded ()
         (set last-student-needed (150 - last-student-needed))
         last-student-needed)
)
(class EveryNMinutes
    [subclass-of Object]
    [ivars interval]
    (class-method new: (n) ((super new) init: n))
    (method init: (n) (set interval n) self)
    (method next () interval)
)
