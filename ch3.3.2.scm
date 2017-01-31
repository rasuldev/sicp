;Constructor:
(make-queue) 

;Selectors:
(empty-queue? <queue>)
(front-queue <queue>) throws exception if queue is empty

;Mutators:
(insert-queue! <queue> <item>)
(delete-queue! <queue>) throws exception if queue is empty

