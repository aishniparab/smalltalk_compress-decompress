
execname := '$RCSfile: outbits.st,v $' =~ '.*: (.*),v.*' at: 1.

""
" Opens a file given as the first argument and writes out bits."
""

nl := Character nl.

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]

"BitStream"
Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
   ]
]

"read file write file"
"after reading put frequencies in a table 0 to 255 characters,
256th is the null one. if val=1 then its EOF"
"for each element in freq-table, if not 0, make a leaf node.
make a priority que for leaf node."
"then remove two smallest elements from priority queue, make a tree"
"smaller is left child, larger is right child. stop when priority queue
is empty"
"also for each char in freqtable which is not zero, make an encoding string
by doing depth-first traversing like 11110"


"might have to modify the above later"
"will also need to make a table with all the frequencies"
"implement all the flags"
"copy paste zeroarray class, leaf class, tree class"


