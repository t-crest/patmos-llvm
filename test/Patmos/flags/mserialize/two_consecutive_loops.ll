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
      - name:            '%input'
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
            opcode:          SUBi
            size:            4
          - index:           1
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           2
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           3
            opcode:          NOP
            size:            4
          - index:           4
            opcode:          SRi
            size:            4
          - index:           5
            opcode:          ADDr
            size:            4
          - index:           6
            opcode:          SRAi
            size:            4
          - index:           7
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           8
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           9
            opcode:          LIl
            size:            8
          - index:           10
            opcode:          MUL
            size:            4
          - index:           11
            opcode:          MFS
            size:            4
          - index:           12
            opcode:          MFS
            size:            4
          - index:           13
            opcode:          SRi
            size:            4
          - index:           14
            opcode:          SRAi
            size:            4
          - index:           15
            opcode:          ADDr
            size:            4
          - index:           16
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           17
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           18
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 1 ]
          - index:           19
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           20
            opcode:          LIl
            size:            8
      - name:            2
        mapsto:          for.body
        predecessors:    [ 1 ]
        successors:      [ 1 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          ADDr
            size:            4
          - index:           4
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           5
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           6
            opcode:          NOP
            size:            4
          - index:           7
            opcode:          ADDi
            size:            4
          - index:           8
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
      - name:            1
        mapsto:          for.cond
        predecessors:    [ 0, 2 ]
        successors:      [ 2, 3 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          CMPLE
            size:            4
          - index:           4
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 2 ]
      - name:            3
        mapsto:          for.end
        predecessors:    [ 1 ]
        successors:      [ 4 ]
        instructions:    
          - index:           0
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 4 ]
          - index:           1
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           2
            opcode:          LIl
            size:            8
      - name:            5
        mapsto:          for.body3
        predecessors:    [ 4 ]
        successors:      [ 4 ]
        loops:           [ 4 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          ADDr
            size:            4
          - index:           4
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           5
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           6
            opcode:          NOP
            size:            4
          - index:           7
            opcode:          ADDi
            size:            4
          - index:           8
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
      - name:            4
        mapsto:          for.cond2
        predecessors:    [ 3, 5 ]
        successors:      [ 5, 6 ]
        loops:           [ 4 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          CMPLE
            size:            4
          - index:           4
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 5 ]
      - name:            6
        mapsto:          for.end5
        predecessors:    [ 4 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           1
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           2
            opcode:          MTS
            size:            4
          - index:           3
            opcode:          ADDi
            size:            4
    subfunctions:    
      - name:            0
        blocks:          [ 0, 2, 1, 3, 5, 4, 6 ]
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
        successors:      [ for.cond ]
        instructions:    
          - index:           0
            opcode:          alloca
          - index:           1
            opcode:          alloca
          - index:           2
            opcode:          alloca
          - index:           3
            opcode:          alloca
          - index:           4
            opcode:          alloca
          - index:           5
            opcode:          alloca
          - index:           6
            opcode:          store
            memmode:         store
          - index:           7
            opcode:          store
            memmode:         store
          - index:           8
            opcode:          load
            memmode:         load
          - index:           9
            opcode:          sdiv
          - index:           10
            opcode:          store
            memmode:         store
          - index:           11
            opcode:          load
            memmode:         load
          - index:           12
            opcode:          sdiv
          - index:           13
            opcode:          store
            memmode:         store
          - index:           14
            opcode:          store
            memmode:         store
          - index:           15
            opcode:          br
      - name:            for.cond
        predecessors:    [ for.inc, entry ]
        successors:      [ for.body, for.end ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          load
            memmode:         load
          - index:           2
            opcode:          icmp
          - index:           3
            opcode:          br
      - name:            for.body
        predecessors:    [ for.cond ]
        successors:      [ for.inc ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          load
            memmode:         load
          - index:           2
            opcode:          add
          - index:           3
            opcode:          store
            memmode:         store
          - index:           4
            opcode:          br
      - name:            for.inc
        predecessors:    [ for.body ]
        successors:      [ for.cond ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          add
          - index:           2
            opcode:          store
            memmode:         store
          - index:           3
            opcode:          br
      - name:            for.end
        predecessors:    [ for.cond ]
        successors:      [ for.cond2 ]
        instructions:    
          - index:           0
            opcode:          store
            memmode:         store
          - index:           1
            opcode:          br
      - name:            for.cond2
        predecessors:    [ for.inc4, for.end ]
        successors:      [ for.body3, for.end5 ]
        loops:           [ for.cond2 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          load
            memmode:         load
          - index:           2
            opcode:          icmp
          - index:           3
            opcode:          br
      - name:            for.body3
        predecessors:    [ for.cond2 ]
        successors:      [ for.inc4 ]
        loops:           [ for.cond2 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          load
            memmode:         load
          - index:           2
            opcode:          add
          - index:           3
            opcode:          store
            memmode:         store
          - index:           4
            opcode:          br
      - name:            for.inc4
        predecessors:    [ for.body3 ]
        successors:      [ for.cond2 ]
        loops:           [ for.cond2 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          add
          - index:           2
            opcode:          store
            memmode:         store
          - index:           3
            opcode:          br
      - name:            for.end5
        predecessors:    [ for.cond2 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
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
        src-block:       for.cond
        dst-block:       1
        src-successors:  [ 3, 4 ]
        dst-successors:  [ 3, 4 ]
      - name:            3
        type:            progress
        src-block:       for.body
        dst-block:       2
        src-successors:  [ 9 ]
        dst-successors:  [ 2 ]
      - name:            4
        type:            progress
        src-block:       for.end
        dst-block:       3
        src-successors:  [ 5 ]
        dst-successors:  [ 5 ]
      - name:            5
        type:            progress
        src-block:       for.cond2
        dst-block:       4
        src-successors:  [ 6, 7 ]
        dst-successors:  [ 6, 7 ]
      - name:            6
        type:            progress
        src-block:       for.body3
        dst-block:       5
        src-successors:  [ 8 ]
        dst-successors:  [ 5 ]
      - name:            7
        type:            progress
        src-block:       for.end5
        dst-block:       6
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            8
        type:            src
        src-block:       for.inc4
        src-successors:  [ 5 ]
      - name:            9
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
    status:          valid
...
