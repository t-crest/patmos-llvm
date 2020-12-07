; RUN: %check-pml
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
---
format:          pml-0.1
triple:          patmos-unknown-unknown-elf
machine-functions: 
  - name:            0
    level:           machinecode
    mapsto:          main
    arguments:       
      - name:            '%cond'
        index:           0
        registers:       [ r3 ]
    hash:            0
    blocks:          
      - name:            0
        mapsto:          entry
        predecessors:    [  ]
        successors:      [ 2, 1 ]
        instructions:    
          - index:           0
            opcode:          MFS
            size:            4
          - index:           1
            opcode:          CMPIEQ
            size:            4
          - index:           2
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 2 ]
      - name:            1
        mapsto:          if.then
        predecessors:    [ 0 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           1
            opcode:          NOP
            size:            4
          - index:           2
            opcode:          ADDi
            size:            4
          - index:           3
            opcode:          MTS
            size:            4
      - name:            2
        mapsto:          if.else
        predecessors:    [ 0 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           1
            opcode:          NOP
            size:            4
          - index:           2
            opcode:          ADDi
            size:            4
          - index:           3
            opcode:          MTS
            size:            4
    subfunctions:    
      - name:            0
        blocks:          [ 0, 1, 2 ]
...
---
format:          pml-0.1
triple:          patmos-unknown-unknown-elf
bitcode-functions: 
  - name:            main
    level:           bitcode
    hash:            0
    blocks:          
      - name:            entry
        predecessors:    [  ]
        successors:      [ if.else, if.then ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then
        predecessors:    [ entry ]
        successors:      [ if.end ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.else
        predecessors:    [ entry ]
        successors:      [ if.end ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end
        predecessors:    [ if.else, if.then ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          ret
...
---
format:          pml-0.1
triple:          patmos-unknown-unknown-elf
relation-graphs: 
  - src:             
      function:        main
      level:           bitcode
    dst:             
      function:        0
      level:           machinecode
    nodes:           
      - name:            0
        type:            entry
        src-block:       entry
        dst-block:       0
        src-successors:  [ 2, 3 ]
        dst-successors:  [ 2, 3 ]
      - name:            1
        type:            exit
      - name:            2
        type:            progress
        src-block:       if.else
        dst-block:       2
        src-successors:  [ 5 ]
        dst-successors:  [ 1 ]
      - name:            3
        type:            progress
        src-block:       if.then
        dst-block:       1
        src-successors:  [ 4 ]
        dst-successors:  [ 1 ]
      - name:            4
        type:            src
        src-block:       if.end
        src-successors:  [ 1 ]
      - name:            5
        type:            src
        src-block:       if.end
        src-successors:  [ 1 ]
    status:          valid
...
