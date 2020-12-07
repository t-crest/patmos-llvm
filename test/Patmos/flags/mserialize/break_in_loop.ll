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
        successors:      [ 1 ]
        instructions:    
          - index:           0
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 1 ]
          - index:           1
            opcode:          LIl
            size:            8
          - index:           2
            opcode:          MFS
            size:            4
      - name:            3
        mapsto:          if.end
        predecessors:    [ 2 ]
        successors:      [ 1 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          SUBi
            size:            4
      - name:            1
        mapsto:          while.cond
        predecessors:    [ 0, 3 ]
        successors:      [ 2, 4 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPILT
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 4 ]
      - name:            2
        mapsto:          while.body
        predecessors:    [ 1 ]
        successors:      [ 4, 3 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          NOP
            size:            4
          - index:           2
            opcode:          CMPEQ
            size:            4
          - index:           3
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 3 ]
      - name:            4
        mapsto:          while.end
        predecessors:    [ 1, 2 ]
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
        blocks:          [ 0, 3, 1, 2, 4 ]
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
        successors:      [ while.cond ]
        instructions:    
          - index:           0
            opcode:          br
      - name:            while.cond
        predecessors:    [ if.end, entry ]
        successors:      [ while.body, while.end ]
        loops:           [ while.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            while.body
        predecessors:    [ while.cond ]
        successors:      [ while.end, if.end ]
        loops:           [ while.cond ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.end
        predecessors:    [ while.body ]
        successors:      [ while.cond ]
        loops:           [ while.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            while.end
        predecessors:    [ while.body, while.cond ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          add
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
        src-successors:  [ 2 ]
        dst-successors:  [ 2 ]
      - name:            1
        type:            exit
      - name:            2
        type:            progress
        src-block:       while.cond
        dst-block:       1
        src-successors:  [ 3, 4 ]
        dst-successors:  [ 3, 4 ]
      - name:            3
        type:            progress
        src-block:       while.body
        dst-block:       2
        src-successors:  [ 5, 4 ]
        dst-successors:  [ 5, 4 ]
      - name:            4
        type:            progress
        src-block:       while.end
        dst-block:       4
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            5
        type:            progress
        src-block:       if.end
        dst-block:       3
        src-successors:  [ 2 ]
        dst-successors:  [ 2 ]
    status:          valid
...
