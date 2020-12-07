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
        successors:      [ 1, 4 ]
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
            opcode:          MFS
            size:            4
          - index:           4
            opcode:          SRi
            size:            4
          - index:           5
            opcode:          ADDr
            size:            4
          - index:           6
            opcode:          ANDl
            size:            8
          - index:           7
            opcode:          SUBr
            size:            4
          - index:           8
            opcode:          CMPIEQ
            size:            4
          - index:           9
            opcode:          BR
            size:            4
            branch-type:     conditional
            branch-delay-slots: 2
            branch-targets:  [ 4 ]
          - index:           10
            opcode:          NOP
            size:            4
          - index:           11
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
      - name:            1
        mapsto:          if.then
        predecessors:    [ 0 ]
        successors:      [ 3 ]
        instructions:    
          - index:           0
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 3 ]
          - index:           1
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           2
            opcode:          LIl
            size:            8
      - name:            2
        mapsto:          for.inc
        predecessors:    [ 3 ]
        successors:      [ 3 ]
        loops:           [ 3 ]
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
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           5
            opcode:          NOP
            size:            4
          - index:           6
            opcode:          ADDr
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
            opcode:          NOP
            size:            4
          - index:           10
            opcode:          ADDi
            size:            4
          - index:           11
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
      - name:            3
        mapsto:          for.cond
        predecessors:    [ 1, 2 ]
        successors:      [ 2, 7 ]
        loops:           [ 3 ]
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
          - index:           5
            opcode:          BRCFNDu
            size:            4
            branch-type:     unconditional
            branch-targets:  [ 7 ]
      - name:            4
        mapsto:          if.else
        predecessors:    [ 0 ]
        successors:      [ 5 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 5 ]
          - index:           1
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           2
            opcode:          LIl
            size:            8
          - index:           3
            opcode:          NOP
            size:            4
      - name:            5
        mapsto:          for.cond2
        predecessors:    [ 4, 6 ]
        successors:      [ 6, 7 ]
        loops:           [ 5 ]
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
            opcode:          SLi
            size:            4
          - index:           3
            opcode:          CMPLE
            size:            4
          - index:           4
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 6 ]
          - index:           5
            opcode:          BRCFNDu
            size:            4
            branch-type:     unconditional
            branch-targets:  [ 7 ]
      - name:            6
        mapsto:          for.inc4
        predecessors:    [ 5 ]
        successors:      [ 5 ]
        loops:           [ 5 ]
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
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           5
            opcode:          NOP
            size:            4
          - index:           6
            opcode:          ADDr
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
            opcode:          NOP
            size:            4
          - index:           10
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 5 ]
          - index:           11
            opcode:          ADDi
            size:            4
          - index:           12
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
      - name:            7
        mapsto:          if.end
        predecessors:    [ 5, 3 ]
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
        blocks:          [ 0, 1, 2, 3, 4 ]
      - name:            5
        blocks:          [ 5, 6 ]
      - name:            7
        blocks:          [ 7 ]
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
            opcode:          alloca
          - index:           1
            opcode:          alloca
          - index:           2
            opcode:          alloca
          - index:           3
            opcode:          alloca
          - index:           4
            opcode:          store
            memmode:         store
          - index:           5
            opcode:          store
            memmode:         store
          - index:           6
            opcode:          load
            memmode:         load
          - index:           7
            opcode:          srem
          - index:           8
            opcode:          icmp
          - index:           9
            opcode:          br
      - name:            if.then
        predecessors:    [ entry ]
        successors:      [ for.cond ]
        instructions:    
          - index:           0
            opcode:          store
            memmode:         store
          - index:           1
            opcode:          br
      - name:            for.cond
        predecessors:    [ for.inc, if.then ]
        successors:      [ for.inc, if.end ]
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
      - name:            for.inc
        predecessors:    [ for.cond ]
        successors:      [ for.cond ]
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
            opcode:          load
            memmode:         load
          - index:           4
            opcode:          add
          - index:           5
            opcode:          store
            memmode:         store
          - index:           6
            opcode:          load
            memmode:         load
          - index:           7
            opcode:          add
          - index:           8
            opcode:          store
            memmode:         store
          - index:           9
            opcode:          br
      - name:            if.else
        predecessors:    [ entry ]
        successors:      [ for.cond2 ]
        instructions:    
          - index:           0
            opcode:          store
            memmode:         store
          - index:           1
            opcode:          br
      - name:            for.cond2
        predecessors:    [ for.inc4, if.else ]
        successors:      [ for.inc4, if.end ]
        loops:           [ for.cond2 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          load
            memmode:         load
          - index:           2
            opcode:          mul
          - index:           3
            opcode:          icmp
          - index:           4
            opcode:          br
      - name:            for.inc4
        predecessors:    [ for.cond2 ]
        successors:      [ for.cond2 ]
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
            opcode:          load
            memmode:         load
          - index:           4
            opcode:          add
          - index:           5
            opcode:          store
            memmode:         store
          - index:           6
            opcode:          load
            memmode:         load
          - index:           7
            opcode:          add
          - index:           8
            opcode:          store
            memmode:         store
          - index:           9
            opcode:          br
      - name:            if.end
        predecessors:    [ for.cond2, for.cond ]
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
        src-successors:  [ 2, 3 ]
        dst-successors:  [ 2, 3 ]
      - name:            1
        type:            exit
      - name:            2
        type:            progress
        src-block:       if.else
        dst-block:       4
        src-successors:  [ 7 ]
        dst-successors:  [ 7 ]
      - name:            3
        type:            progress
        src-block:       if.then
        dst-block:       1
        src-successors:  [ 4 ]
        dst-successors:  [ 4 ]
      - name:            4
        type:            progress
        src-block:       for.cond
        dst-block:       3
        src-successors:  [ 5, 6 ]
        dst-successors:  [ 5, 6 ]
      - name:            5
        type:            progress
        src-block:       for.inc
        dst-block:       2
        src-successors:  [ 4 ]
        dst-successors:  [ 4 ]
      - name:            6
        type:            progress
        src-block:       if.end
        dst-block:       7
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            7
        type:            progress
        src-block:       for.cond2
        dst-block:       5
        src-successors:  [ 8, 6 ]
        dst-successors:  [ 8, 6 ]
      - name:            8
        type:            progress
        src-block:       for.inc4
        dst-block:       6
        src-successors:  [ 7 ]
        dst-successors:  [ 7 ]
    status:          valid
...
