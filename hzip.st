#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f
"""
ehobbs@unix.ucsc.edu aparab@unix.ucsc.edu
iexecname := ’$RCSfile: outbits.st,v $’ =˜ ’.*: (.*),v.*’ at: 1.
"""

nl := Character nl.

Object subclass: Leaf [
   nl := Character nl.
   |char count|
   char [ ^ char ]
   count [ ^ count ]

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
   ]

   <= other [
      ^ (count < other count) | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   "changed this from mackey's original leaf code, this takes in a
    zeroarray encoding table, and this is called at the end of recursion
    from the tree function of the same name. this updates the encoding table
    for this leaf"
   inorder: visitor prefix: string eTab: eTab args: Args[     
     "visitor value: char value: string."
   |index|
      Args dFlag = true ifTrue:[Args outF << self char << ' ' << self count << nl.].

      index := visitor char asciiValue.
      eTab at: index put: string.
   ]

   "
   encode: char: indexChar String: binstr [
     return string, and put into zeroarray
   ]"
]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   "This constructor takes in a priority queue, and spits out a new tree
    with the leaves from thats priority queue."
   Tree class >> new: PriQ [
      |first second newNode newChar newCount|
      [PriQ size > 1]whileTrue:[
         first := PriQ removeFirst.
         second := PriQ removeFirst.
         newCount := first count + second count.
         newChar := first char.
         newNode := Tree new: newChar count: newCount left: first right: second.
         PriQ add: newNode.
      ]
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string eTab: eTab dFlag: dFlag[
      dFlag ifTrue:[Args outF << self left << ' ' << self right << nl.]
      left inorder: visitor prefix: string, '0' eTab: eTab dFlag: dFlag.
      right inorder: visitor prefix: string, '1' eTab: eTab dFlag: dFlag.
   ]
   
   "This will be called on a full tree, and it will return a zeroarray full of the strings
    for encoding each ascii character"
   encode: dFlag[
   |encodeTab encodeTree|
      encodeTab := ZeroArray new: 257.
      encodeTree := PriQ removeLast.
      "Depth First Search
      left encode char: indexChar String: binstr + ""0"".
      right encode char: indexChar String: binstr + ""1"".
      "
      self inorder: self prefix: '' eTab: encodeTab dFlag: dFlag.
      ^ encodeTab.
   ]
  
  binTreeDump: aBitStream [
      aBitStream putBit: 0.
      (1 to: 8) do: [:index | 
          aBitStream putBit: ( (self char asInteger) bitAt: index) ].
      char asInteger = 0 ifTrue: [
         aBitStream putBit: 0.
      ].
      char asInteger = 256 ifTrue: [
         aBitStream putBit: 1.
      ]

    ]

]

Object subclass: ZeroArray [
nl := Character nl.
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

  "This method is called by the frequency table once it is filled,
   and it returns a priority queue full of Leaves."
  makeQueue [
  |PriQ aLeaf aCount aChar|
     PriQ := SortedCollection new.
     (0 to: (self size) - 1) do: [:index |
        aCount := self at: index.
        "aCount = 0 ifFalse:["
        aCount isInteger ifTrue:[
            "not sure if Character is right"
            aChar := index Character.
            aLeaf := Leaf new: aChar count: aCount. 
            PriQ add: aLeaf.
        ].
     ].
     ^ PriQ.
  ]
  "called by the encoding table, this function will print it out."
  printTable: frequencyTable [
     (0 to: self size - 1)
   do: [:index |
       |char isMore32 isLess127 count string|
       isMore32 := index > 32.
       isLess127 := index < 127.
       count := frequencyTable at: index.
       string := self at: index.
       count isInteger ifTrue: [
          isMore32 & isLess127 ifTrue: [
             char := index asCharacter.
             stdout << char << ' ' << count << ' ' << string << nl.
          ] ifFalse: [
             stdout << index << ' ' << count << ' ' << string << nl.
          ].
       ].
   ].
   ]

]

Object subclass: BitStream [
   |bitIndex byte myStream stringIndex writeIndex writeByte|
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
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
   currByte [
      ^byte
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
      "(1 to: theString size - 1)" " This may be off by one. "
      "do: [:index | self putBit: (theString at: index)]."
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
"

FileStream extend [
   nl := Character nl.
   catFile [
      |count|
      count := 0.
      [self atEnd not] whileTrue: [
         count := count + 1.
         count asString size to: 5 do: [:skip| stdout << ' '].
         stdout << count << ': ' << self nextLine << nl.
      ]
   ]
   makeTable: freqTab dFlag: dFlag outfile: outF [
  "Arguments: args["
  "this function will read the file one character at a time, 
   and put it into an array of size 256, creating our frequency table
   It takes in the array as a parameter and the file to read from"
   |currChar index value|
   [self atEnd not] whileTrue: [
      currChar := self next.
      currChar = nil ifTrue: [freqTab at: 257 put: 1.]
      ifFalse: [
         index := currChar asciiValue.
         value := freqTab at: index.
         freqTab at: index put: [value + 1]. "this might need precedence"
         dFlag = true ifTrue:
            [outF << index << value.]
      ]
   ]
   ]
]
"
Object subclass: Arguments [
   |dFlag cFlag uFlag tFlag inF outF|
   dFlag [ ^ dFlag ]
   cFlag [ ^ cFlag ]
   uFlag [ ^ uFlag ]
   tFlag [ ^ tFlag ]
   inF   [ ^ inF   ]
   outF  [ ^ outF  ]

   setdFlag: bool [dFlag := bool.]
   setcFlag: bool [cFlag := bool.]
   setuFlag: bool [uFlag := bool.]
   settFlag: bool [tFlag := bool.]
   setinF: filn [inF := filn.]
   setoutF: filo [outF := filo.]

   Arguments class >> new [
     ""setdFlag: false.
      setcFlag: false.
      setuFlag: false.
      settFlag: false.
      setinF: ''.
      setoutF: stdout.
      self shouldNotImplement.
   ]

   Arguments class >> new: dFlagArg cFlag: cFlagArg uFlag: uFlagArg tFlag: tFlagArg inF: infile outF: outfile [
      self setdFlag: dFlagArg.
      self setcFlag: cFlagArg.
      self setuFlag: uFlagArg.
      self settFlag: tFlagArg.
      self setinF: (FileStream open: infile mode FileStream read).
      self setoutF: stdout.
   ]     

   closeFiles [
      self inF close.
      self outF close.
   ]

]"

"MAIN"
"|Args freqTab PriQ ETree EncodeTab currChar index value "
" dFlag cFlag uFlag tFlag readFile writeFile|"

|freqTab PriQ ETree EncodeTab currChar index value readFile writeFile inF outF|
Smalltalk arguments size = 0
   ifTrue: [stdout << 'Not enough arguments' ]
   ifFalse: [
         
        "need to open the file in a fileStream, and then make a zeroArray thats gonna
         be our frequency table, then call makeTable on those arguments. Next, we want to call
         makeQueue on our frequency table, and well have a priority queue. So, we can use 
         the Tree constuctor with this priority queue to make an entire tree. Now, on that tree, 
         we call encode to make the encoding table. Once have our encoding table, we can loop through
         the input file again, and encode every char."
         

         dFlag := false.
         cFlag := false.
         uFlag := false.
         tFlag := false.
         inF := ''.
	"stdout << 'hjfjhfjh' << nl."
         outF := stdout.

"CHANGE THIS CODE"
   (1 to: Smalltalk arguments size)
   do: [:index |
       aArg := Smalltalk arguments at: index.
       (aArg includes: $-) ifTrue: [
           (aArg includes: $d) ifTrue: [dFlag := true.].
           (aArg includes: $c) ifTrue: [cFlag := true.].
           (aArg includes: $t) ifTrue: [uFlag := true.].
           (aArg includes: $u) ifTrue: [tFlag := true.].
       ] ifFalse: [
           "This assumes the next arguments are files"
           "No testing is done for non-valid options because this is not"
           "tested in the score files"
           (inF isEmpty) ifTrue: [
               inF := aArg.
           ] ifFalse: [
               outF := aArg.
               writeFile := FileStream open: outfile
                             mode: FileStream write.
          ]
        ]
     ].

       
        " Args := Arguments new: Smalltalk arguments at: 2 indexOf 'd' ifAbsent : [0] = 0 not
                         cFlag: Smalltalk arguments at: 2 indexOf 'c' ifAbsent : [0] = 0 not
                         uFlag: Smalltalk arguments at: 2 indexOf 'u' ifAbsent : [0] = 0 not
                         tFlag: Smalltalk arguments at: 2 indexOf 't' ifAbsent : [0] = 0 not
                           inF: Smalltalk arguments at: 3
                          outF: Smalltalk arguments size = 4 
                                   ifTrue: [Smalltalk arguments at: 4]
                                   ifFalse: [stdout]"
         
         readFile := FileStream open: inF
                                mode: FileStream read.
         
         "writeFile := FileStream open: outfile
                                 mode: FileStream write.
         "
         freqTab := ZeroArray new: 257.
         
         readFile makeTable: freqTab dFlag: dFlag outfile: outF.
         PriQ := freqTab makeQueue.
         ETree := Tree new: PriQ.
         EncodeTab := ETree encode: dFlag.

         "Print to writeFile"
         
cFlag ifTrue: [ "Perform compression."
   |encString|
   aBitStream := BitStream new: writeFile.
   readFile reset.
   ETree binTreeDump: aBitStream.
   aBitStream putBit: 1. "This will pop the last thing off the stack when we decode."
   [readFile atEnd not] whileTrue: [
      |index|
      index := readFile next asInteger.
      encString := (encodeTable at: index).
      aBitStream putString: encString.
   ].
   encString := encodeTable at: 256.
   aBitStream putString: encString.
   aBitStream padOut.
   readFile close.
   writeFile close.
].



   tFlag ifTrue: [
      EncodeTab printTable: freqTab.
      readFile close.
   ].

    "  [Args inF atEnd not] whileTrue: [
         currChar := Args inF next.
         ""need to print out the encode bit by bit""
         currChar = nil ifTrue: [Args outF nextPut EncodeTab at: 257.]
         ifFalse: [
            index := currChar asciiValue.
            value := EncodeTab at: index.
            Args outF nextPut value.
         ]
      ]


        ""loop thru file again and print the encoded version of each char in the outfile""

         filename = '-'

            ifTrue: [stdin catFile]
            ifFalse: [
               [
                  |infile|
                  infile := FileStream open: filename
                                       mode: FileStream read.
                  infile catFile.
                  infile close.
               ] on: SystemExceptions.FileError do: [:signal |
                  |errno|
                  errno := File errno.
                  stdout flush.
                  stderr << execname << ': ' << filename << ': '
                         << signal messageText << nl.
                  stderr flush.
               ]
            ]"
      ]
