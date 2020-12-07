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
      - name:            '%iteration_count'
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
            opcode:          MOV
            size:            4
          - index:           1
            opcode:          LIl
            size:            8
          - index:           2
            opcode:          LIin
            size:            4
          - index:           3
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 1 ]
          - index:           4
            opcode:          MOV
            size:            4
          - index:           5
            opcode:          MFS
            size:            4
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
            opcode:          NOP
            size:            4
          - index:           2
            opcode:          ADDr
            size:            4
          - index:           3
            opcode:          ADDr
            size:            4
          - index:           4
            opcode:          ADDi
            size:            4
      - name:            1
        mapsto:          for.cond
        predecessors:    [ 0, 2 ]
        successors:      [ 2, 3 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
          - index:           1
            opcode:          CMPLE
            size:            4
          - index:           2
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 2 ]
      - name:            3
        mapsto:          for.end
        predecessors:    [ 1 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          MTS
            size:            4
          - index:           1
            opcode:          RETND
            size:            4
            branch-type:     return
    subfunctions:    
      - name:            0
        blocks:          [ 0, 2, 1, 3 ]
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
            opcode:          br
      - name:            for.cond
        predecessors:    [ for.inc, entry ]
        successors:      [ for.body, for.end ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          phi
          - index:           2
            opcode:          phi
          - index:           3
            opcode:          add
          - index:           4
            opcode:          icmp
          - index:           5
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
            opcode:          br
      - name:            for.inc
        predecessors:    [ for.body ]
        successors:      [ for.cond ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          add
          - index:           2
            opcode:          add
          - index:           3
            opcode:          br
      - name:            for.end
        predecessors:    [ for.cond ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          ret
flowfacts:       
  - scope:           
      function:        main
      loop:            for.cond
    lhs:             
      - factor:          1
        program-point:   
          function:        main
          block:           for.cond
    op:              less-equal
    rhs:             2147483648
    level:           bitcode
    origin:          llvm.bc
    classification:  loop-global
  - scope:           
      function:        main
      loop:            for.cond
    lhs:             
      - factor:          1
        program-point:   
          function:        main
          block:           for.cond
    op:              less-equal
    rhs:             '(1 + (0 smax %iteration_count))'
    level:           bitcode
    origin:          llvm.bc
    classification:  loop-global
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
        src-successors:  [ 5 ]
        dst-successors:  [ 2 ]
      - name:            4
        type:            progress
        src-block:       for.end
        dst-block:       3
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            5
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
    status:          valid
...
