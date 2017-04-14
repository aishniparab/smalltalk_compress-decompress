#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
nl := Character nl.

"Object Classes"
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
     myStream nextPutByte: currentByte.
     self clearByte.
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
   ]
]


Object subclass: BitStream [
   |stringIndex bitIndex byte myStream writeIndex writeByte|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
      writeIndex := 9.
      writeByte := 0.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   putBit: theBit [
      writeIndex := writeIndex - 1.
      writeByte := writeByte bitAt: writeIndex put: theBit.
      writeIndex = 1 ifTrue: [
         myStream nextPutByte: writeByte.
         writeByte := 0.
         writeIndex := 9.
      ].
   ]
   
   putString: theString [
      theString do: [:index|
         (index asString = '0')
            ifTrue: [self putBit: 0]
            ifFalse: [self putBit: 1].
      ].
   ]
   padOut [
      [writeIndex > 1 & writeIndex < 9] whileFalse: [ 
         self putBit: 0. 
         writeIndex := writeIndex - 1.].
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

"
Extending Filestream to put in frequency table
"
FileStream extend [ 
    createFreqTable [ 
    |freqtable|
      freqtable := ZeroArray new: 257.
      [self atEnd not] whileTrue: [
         |ordChar|
         ordChar := self next asInteger.
         freqtable incr: ordChar.
      ].
      freqtable incr: 256.
      ^ freqtable
    ]
]


Object subclass: ZeroArray [
    |theArray|

    ZeroArray class >> new [
        self shouldNotImplement.
    ]

    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]

    init: size [
        theArray := Array new: size.
    ]

    size [
        ^ theArray size.
    ]

    at: index [
        ^ theArray at: index + 1.
    ]

    at: index put: value [
        ^ theArray at: index + 1 put: value.
    ]

    incr: index [
        (self at: index) isNil
            ifTrue: [ self at: index put: 0].
        self at: index put: (self at: index) + 1.
    ]

    keysAndValuesDo: aBlock [
        (0 to: (self size) - 1) do: [:index |
            aBlock value: index value: (self at: index).
        ]
    ]
]

"
Leaf Class
"
Object subclass: Leaf [
    |char count isLeaf|
    char [ ^ char ]
    count [ ^ count ]
    isLeaf [ ^ true ]
    Leaf class >> new [
        self shouldNotImplement
    ]

    Leaf class >> new: aChar count: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]

    setChar: aChar andCount: aCount [
        char := aChar.
        count := aCount.
        isLeaf := true.
    ]

    <= other [
        ^ (count < other count)
        | ((count = other count) & (char <= other char)) 
    ]

    printBase: aStream [
        |fixhl|
        ^ aStream << self class << '(' << char << ',' << count
    ]
    printOn: aStream [
        (self printBase: aStream) << ')'.
    ]

    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]
    
    postorder: visitor [
        visitor value: char value: isLeaf.
    ]

    
    binTreeDump: aBitstream [
        "leaf node put 0 followed by the 8 bits of the corresponding
        byte"
      
      aBitstream putBit: 0.
      (1 to: 8) do: [:index | 
          aBitstream putBit: ( (self char asInteger) bitAt: index) ].
      "add 8th bit for special cases"
      char asInteger = 0 ifTrue: [
         aBitstream putBit: 0.
      ].
      char asInteger = 256 ifTrue: [
         aBitstream putBit: 1.
      ]
      

    ]

]

"
Extending leaf class for Object Tree
"
Leaf subclass: Tree [
   |left right|
   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
       |result|
       result := super new: aChar count: aCount.
       result setLeft: aLeft andRight: aRight.
       ^ result
   ]

   isLeaf [ ^ false ]

   setLeft: aLeft andRight: aRight [
       left := aLeft.
       right := aRight.
       isLeaf := false.
   ]
   right [ ^ right ]
   left [ ^ left ]

   printOn: aStream [
       (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
       left inorder: visitor prefix: string, '0'.
       right inorder: visitor prefix: string, '1'.
   ]
   
   postorder: visitor [
        left postorder: visitor.
        right postorder: visitor.
        visitor value: char value:isLeaf.
    ]

   
   
   binTreeDump: aBitstream [
      left binTreeDump: aBitstream. "leaf nodes"
      right binTreeDump: aBitstream.
      aBitstream putBit: 1. "interior node"
   ]
]


"
Options
"
d_flag := false.
t_flag := false.
c_flag := false.
u_flag := false.
inputfile := ''.
outputfile := stdout.

"
Get hzip arguments and set the flags
"
(1 to: Smalltalk arguments size)
do: [:index |
    argv := (Smalltalk arguments at: index).
    (argv includes: $-) ifTrue: [
        (argv includes: $d) ifTrue: [d_flag := true.].
        (argv includes: $t) ifTrue: [t_flag := true.].
        (argv includes: $c) ifTrue: [c_flag := true.].
        (argv includes: $u) ifTrue: [u_flag := true.].
    ] ifFalse: [
        (inputfile isEmpty) ifTrue: [
            inputfile := argv.
        ] ifFalse: [
            outputfile :=  argv.
        ]
    ]
].

"
Filestream the inputfile
"
|readfile|
readfile := FileStream open: inputfile 
                       mode: FileStream read.

"
If writefile is defined then open outputfile file stream
"
|writefile|
writefile := stdout.
outputfile = stdout ifFalse: [
        writefile := FileStream open: outputfile
                        mode: FileStream write.
    ].

"
Read in the input file and create a frequency table, counting the number
of times each character appears on input. The frequency table is indexed
from 0 to 255 with characters. Add entry 256 with a count of 1 to 
indicate EOF.
"
|frequencyTable|
u_flag ifFalse: [frequencyTable := readfile createFreqTable.].
readfile close.

"
Dump frequency table for debug
"
d_flag ifTrue: [
    nl display.
    'Frequency Table Dump:' display.
    nl display.
    (0 to: frequencyTable size - 1)
    do: [:index |
        index display.
        ' ' display.
        item := frequencyTable at: index.
        item display.
        nl display.
    ].
    nl display.
].

"
Iterate over the frequency table, and for each non-zero element, create
a leaf node and insert that leaf node into a priority queue, with the
character and the count. In Smalltalk, use a SortedCollection. The
counts take precedence, but if two entries have the same count, the one 
with the smaller character (lexicographically) is considered smaller.
"
|priorityQueue|
priorityQueue := SortedCollection new.
u_flag ifFalse:[
   (0 to: frequencyTable size - 1)
   do: [ :index |
      |aLeaf aCount aChar|
      aCount := frequencyTable at: index.
      aCount isInteger ifTrue: [
          aChar := index.
          aLeaf := Leaf new: aChar  count: aCount.
          priorityQueue add: aLeaf.
      ].
   ].
].

"
Dump priority queue for debug
"
d_flag ifTrue: [
    nl display.
    'Priority Queue Dump:' display.
    nl display.
    priorityQueue inspect.
    nl display.
].

"
Repeatedly remove the two smallest elements from the priority queue,
creating a new tree which is then entered into the priority queue. The 
smaller tree or leaf removed becomes the left child, and the larger the
right child. The character in the new tree is the left child's 
character. This process stops when there is only one tree left and the 
priority queue is empty
"

u_flag ifFalse: [
   [priorityQueue size > 1] whileTrue: [
       |smallestElement secondSmallestElement aTree treeChar treeCount|
       smallestElement := priorityQueue removeFirst.
       secondSmallestElement := priorityQueue removeFirst.
       treeChar := smallestElement char.
       treeCount := smallestElement count + secondSmallestElement count.
       aTree := Tree new: treeChar 
            count: treeCount left: smallestElement 
                             right: secondSmallestElement.
       priorityQueue add: aTree.
   ].
].

d_flag ifTrue: [
    nl display.
    'Priority Queue Dump After Adding Trees:' display.
    nl display.
    priorityQueue inspect.
    nl display.
].

"
For each character that has appeared as non-zero in the frequency table,
construct an encoding string, using a depth-first traversal. The
encoding string is a sequence of bits indicating the path from the root
to a leaf.
"
|encodingTree encodingTable binaryString|
u_flag ifFalse: [
    encodingTree := priorityQueue removeLast.
    encodingTable := ZeroArray new: 257.
    encodingTree inorder: [:char :string |
      |index|
      index := char asInteger.
      encodingTable at: index put: string.
   ] prefix: ''.
   
    binaryString := ''.
    encodingTree postorder:[:char :isLeaf |
      isLeaf  ifFalse:[binaryString:=binaryString,'1'.]
      ifTrue:[
        binaryCode:=''.
        char = 256
        ifTrue:[binaryCode:='000000001']
        ifFalse:[
          char=0 ifTrue:[binaryCode:='000000000']
          ifFalse:[
            8 to:1 by: -1 do:[:i|
              binaryCode:=binaryCode,((char bitAt:i) printString).
            ]
          ].               
        ].
        binaryString:=(binaryString,'0',binaryCode).
      ].
    ].
].


d_flag ifTrue: [
    nl display.
    'Encoded Bitstream:' display.
    nl display.
    stdout << binaryString.
    nl display.
].

"
If the -t option is specified, write out the encoding table sorted by
character. The first column is a single character, if printable, or an
integer if not. The second column is the frequency for that character.
The third column is a sequence of 0 and 1 characters indicating the
encoding. Format should appear as if done by the C format items 
%3d %5d %s or %3c %5d %s. Then stop.
"
t_flag ifTrue: [
    nl display.
    'Encoding Table:' display.
    nl display.
    (0 to: encodingTable size - 1)
    do: [:index |
       |char count string|
       count := frequencyTable at: index.
       string := encodingTable at: index.
       printableLowerBound := index > 31.
       printableUpperBound := index < 127.
       count isInteger ifTrue: [
          printableLowerBound & printableUpperBound ifTrue: [
             char := index asCharacter.
             char asString size to: 3 do: [:skip| stdout << ' '].
             stdout << char << ' ' .
             count asString size to: 5 do: [:skip| stdout << ' '].
             stdout << count << ' ' << string << nl.
          ] ifFalse: [
            index asString size to: 3 do: [:skip| stdout << ' '].
             stdout << index << ' ' .
             count asString size to: 5 do: [:skip| stdout << ' '].
             stdout << count << ' ' << string << nl.
          ].
       ].
   ]. 
   nl display.
].

c_flag ifTrue: [
    |outBytes|
    outBytes := OutBits new: writefile.
    1 to: (binaryString size) by:1 do: [:index|
        bit:= ((binaryString at:index) asInteger) - 48.
        outBytes writeBit: bit.
    ].
    readfile := FileStream open: inputfile 
                   mode: FileStream read.
    outBytes writeBit: 1.
    [readfile atEnd not] whileTrue: [
      |index|
      index := readfile next asInteger.
      encodedString := (encodingTable at: index).
      1 to: (encodedString size) by:1 do: [:index|
        bit := ((encodedString at:index) asInteger) - 48.
        outBytes writeBit: bit.
      ].
    ].
    encodedString := (encodingTable at: 256).
    1 to: (encodedString size) by:1 do: [:index|
    bit := ((encodedString at:index) asInteger) - 48.
    outBytes writeBit: bit.
    ]. 
    outBytes flushByte.
    readfile close.
    writefile close.
].

u_flag ifTrue: [ 
    readfile := FileStream open: inputfile 
                       mode: FileStream read.
    inBitStream := BitStream new: readfile.
    stack := OrderedCollection new.
    treeComplete := false.
    eof_flag := false.
        [(inBitStream atEnd not) & (eof_flag not)] whileTrue: [
          treeNode := inBitStream nextBit.
          leafCharacter := 0.
          treeComplete ifFalse:[
            treeNode = 0 ifTrue:[
              8 to:1 by:-1 do: [:i|
                bit := inBitStream nextBit.
                leafCharacter := leafCharacter bitAt:i put:bit.
               ].
               leafCharacter = 0 ifTrue:[
                 extraBit := inBitStream nextBit.
                 extraBit = 1 ifTrue:[
                  leafCharacter := leafCharacter bitAt:9 put:1.
                 ].
               ].
              stack add: (Leaf new: leafCharacter count: '').
            ].
            treeNode = 1 ifTrue:[
               stack size > 1 ifTrue:[
                 childRight := stack removeLast.
                 childLeft := stack removeLast.
                 newtree := (Tree new: nil count: '' left: childLeft 
                   right: childRight).
                 stack add: newtree.
               ] ifFalse:[
                 treeComplete := true.
                 decodingTree := stack removeLast.
                 node := decodingTree.
               ].
            ].
         ] ifTrue:[
           node isLeaf ifTrue:[
             (node char) = 256 ifTrue:[
               eof_flag := true.
             ] ifFalse:[
               stdout nextPutByte: node char.
               node := decodingTree.
             ].
           ]ifFalse:[
             treeNode = 0 ifTrue:[
             node := node left.
             node isLeaf ifTrue:[
               eof_flag not ifTrue:[  
                 (node char) = 256 ifTrue:[
                   eof_flag := true.
                 ] ifFalse:[
                 writefile nextPutByte: node char.
                 node := decodingTree.
                 ].
               ].
             ]
           ] ifFalse:[
             node := node right.
             node isLeaf ifTrue:[
             eof_flag not ifTrue:[
               (node char) = 256 ifTrue:[
                 eof_flag := true.
               ] ifFalse:[
                 writefile nextPutByte: node char.
                 node := decodingTree.
               ].
             ].
           ]
         ].
       ].
     ].
   ].
   readfile close.
   writefile close.
].
