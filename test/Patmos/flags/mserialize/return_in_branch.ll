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
      - name:            '%x'
        index:           0
        registers:       [ r3 ]
    hash:            0
    blocks:          
      - name:            0
        mapsto:          entry
        predecessors:    [  ]
        successors:      [ 1, 2 ]
        instructions:    
          - index:           0
            opcode:          LIl
            size:            8
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          MFS
            size:            4
          - index:           3
            opcode:          CMPLE
            size:            4
          - index:           4
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
            opcode:          LIl
            size:            8
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           3
            opcode:          NOP
            size:            4
          - index:           4
            opcode:          ADDr
            size:            4
          - index:           5
            opcode:          MTS
            size:            4
      - name:            2
        mapsto:          if.else
        predecessors:    [ 0 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          LIl
            size:            8
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           3
            opcode:          NOP
            size:            4
          - index:           4
            opcode:          SUBr
            size:            4
          - index:           5
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
        successors:      [ if.then, if.else ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then
        predecessors:    [ entry ]
        successors:      [ return ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else
        predecessors:    [ entry ]
        successors:      [ return ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          sub
          - index:           2
            opcode:          br
      - name:            return
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
        src-block:       return
        src-successors:  [ 1 ]
      - name:            5
        type:            src
        src-block:       return
        src-successors:  [ 1 ]
    status:          valid
...
