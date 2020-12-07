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
            opcode:          MOV
            size:            4
          - index:           1
            opcode:          LIi
            size:            4
          - index:           2
            opcode:          LIi
            size:            4
          - index:           3
            opcode:          LIi
            size:            4
          - index:           4
            opcode:          LIi
            size:            4
          - index:           5
            opcode:          LIi
            size:            4
          - index:           6
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 1 ]
          - index:           7
            opcode:          MOV
            size:            4
          - index:           8
            opcode:          MFS
            size:            4
      - name:            16
        mapsto:          if.else29
        predecessors:    [ 2 ]
        successors:      [ 1 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
          - index:           1
            opcode:          ADDi
            size:            4
      - name:            1
        mapsto:          for.cond
        predecessors:    [ 0, 16, 11 ]
        successors:      [ 2, 17 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLE
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 17 ]
      - name:            2
        mapsto:          for.body
        predecessors:    [ 1 ]
        successors:      [ 3, 16 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLT
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 16 ]
      - name:            3
        mapsto:          if.then
        predecessors:    [ 2 ]
        successors:      [ 4, 15 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLT
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 15 ]
      - name:            4
        mapsto:          if.then3
        predecessors:    [ 3 ]
        successors:      [ 5, 14 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLT
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 14 ]
      - name:            5
        mapsto:          if.then5
        predecessors:    [ 4 ]
        successors:      [ 6, 13 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLT
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 13 ]
      - name:            6
        mapsto:          if.then7
        predecessors:    [ 5 ]
        successors:      [ 7, 12 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLT
            size:            4
          - index:           1
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 12 ]
      - name:            7
        mapsto:          if.then9
        predecessors:    [ 6 ]
        successors:      [ 8 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          CMPLT
            size:            4
          - index:           1
            opcode:          ADDi
            size:            4
          - index:           2
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 8 ]
          - index:           3
            opcode:          SUBi
            size:            4
          - index:           4
            opcode:          SUBi
            size:            4
      - name:            15
        mapsto:          if.else25
        predecessors:    [ 3 ]
        successors:      [ 11 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
          - index:           1
            opcode:          BRNDu
            size:            4
            branch-type:     unconditional
            branch-targets:  [ 11 ]
      - name:            14
        mapsto:          if.else21
        predecessors:    [ 4 ]
        successors:      [ 10 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
          - index:           1
            opcode:          BRNDu
            size:            4
            branch-type:     unconditional
            branch-targets:  [ 10 ]
      - name:            13
        mapsto:          if.else17
        predecessors:    [ 5 ]
        successors:      [ 9 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
          - index:           1
            opcode:          BRNDu
            size:            4
            branch-type:     unconditional
            branch-targets:  [ 9 ]
      - name:            12
        mapsto:          if.else13
        predecessors:    [ 6 ]
        successors:      [ 8 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
      - name:            8
        mapsto:          '(null)'
        predecessors:    [ 12, 7 ]
        successors:      [ 9 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          SUBi
            size:            4
      - name:            9
        mapsto:          '(null)'
        predecessors:    [ 8, 13 ]
        successors:      [ 10 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          SUBi
            size:            4
      - name:            10
        mapsto:          '(null)'
        predecessors:    [ 9, 14 ]
        successors:      [ 11 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          SUBi
            size:            4
      - name:            11
        mapsto:          '(null)'
        predecessors:    [ 10, 15 ]
        successors:      [ 1 ]
        loops:           [ 1 ]
        instructions:    
          - index:           0
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 1 ]
          - index:           1
            opcode:          SUBi
            size:            4
          - index:           2
            opcode:          ADDi
            size:            4
      - name:            17
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
        blocks:          [ 0, 16, 1, 2, 3, 4, 5, 6, 7, 15, 14, 13, 12, 
                           8, 9, 10, 11, 17 ]
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
            opcode:          icmp
          - index:           3
            opcode:          br
      - name:            for.body
        predecessors:    [ for.cond ]
        successors:      [ if.then, if.else29 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then
        predecessors:    [ for.body ]
        successors:      [ if.then3, if.else25 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then3
        predecessors:    [ if.then ]
        successors:      [ if.then5, if.else21 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then5
        predecessors:    [ if.then3 ]
        successors:      [ if.then7, if.else17 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then7
        predecessors:    [ if.then5 ]
        successors:      [ if.then9, if.else13 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then9
        predecessors:    [ if.then7 ]
        successors:      [ if.then11, if.else ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            if.then11
        predecessors:    [ if.then9 ]
        successors:      [ if.end ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.else
        predecessors:    [ if.then9 ]
        successors:      [ if.end ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end
        predecessors:    [ if.else, if.then11 ]
        successors:      [ if.end15 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else13
        predecessors:    [ if.then7 ]
        successors:      [ if.end15 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end15
        predecessors:    [ if.else13, if.end ]
        successors:      [ if.end19 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else17
        predecessors:    [ if.then5 ]
        successors:      [ if.end19 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end19
        predecessors:    [ if.else17, if.end15 ]
        successors:      [ if.end23 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else21
        predecessors:    [ if.then3 ]
        successors:      [ if.end23 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end23
        predecessors:    [ if.else21, if.end19 ]
        successors:      [ if.end27 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else25
        predecessors:    [ if.then ]
        successors:      [ if.end27 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end27
        predecessors:    [ if.else25, if.end23 ]
        successors:      [ if.end31 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else29
        predecessors:    [ for.body ]
        successors:      [ if.end31 ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end31
        predecessors:    [ if.else29, if.end27 ]
        successors:      [ for.inc ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          br
      - name:            for.inc
        predecessors:    [ if.end31 ]
        successors:      [ for.cond ]
        loops:           [ for.cond ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
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
    rhs:             '(1 + (0 smax %x))'
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
        src-successors:  [ 5, 6 ]
        dst-successors:  [ 5, 6 ]
      - name:            4
        type:            progress
        src-block:       for.end
        dst-block:       17
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            5
        type:            progress
        src-block:       if.else29
        dst-block:       16
        src-successors:  [ 56 ]
        dst-successors:  [ 2 ]
      - name:            6
        type:            progress
        src-block:       if.then
        dst-block:       3
        src-successors:  [ 7, 8 ]
        dst-successors:  [ 7, 8 ]
      - name:            7
        type:            progress
        src-block:       if.else25
        dst-block:       15
        src-successors:  [ 52 ]
        dst-successors:  [ 55 ]
      - name:            8
        type:            progress
        src-block:       if.then3
        dst-block:       4
        src-successors:  [ 9, 10 ]
        dst-successors:  [ 9, 10 ]
      - name:            9
        type:            progress
        src-block:       if.else21
        dst-block:       14
        src-successors:  [ 46 ]
        dst-successors:  [ 50 ]
      - name:            10
        type:            progress
        src-block:       if.then5
        dst-block:       5
        src-successors:  [ 11, 12 ]
        dst-successors:  [ 11, 12 ]
      - name:            11
        type:            progress
        src-block:       if.else17
        dst-block:       13
        src-successors:  [ 38 ]
        dst-successors:  [ 43 ]
      - name:            12
        type:            progress
        src-block:       if.then7
        dst-block:       6
        src-successors:  [ 13, 14 ]
        dst-successors:  [ 13, 14 ]
      - name:            13
        type:            progress
        src-block:       if.else13
        dst-block:       12
        src-successors:  [ 28 ]
        dst-successors:  [ 34 ]
      - name:            14
        type:            progress
        src-block:       if.then9
        dst-block:       7
        src-successors:  [ 15, 16 ]
        dst-successors:  [ 24 ]
      - name:            15
        type:            src
        src-block:       if.then11
        src-successors:  [ 17 ]
      - name:            16
        type:            src
        src-block:       if.else
        src-successors:  [ 17 ]
      - name:            17
        type:            src
        src-block:       if.end
        src-successors:  [ 18 ]
      - name:            18
        type:            src
        src-block:       if.end15
        src-successors:  [ 19 ]
      - name:            19
        type:            src
        src-block:       if.end19
        src-successors:  [ 20 ]
      - name:            20
        type:            src
        src-block:       if.end23
        src-successors:  [ 21 ]
      - name:            21
        type:            src
        src-block:       if.end27
        src-successors:  [ 22 ]
      - name:            22
        type:            src
        src-block:       if.end31
        src-successors:  [ 23 ]
      - name:            23
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
      - name:            24
        type:            dst
        dst-block:       8
        dst-successors:  [ 25 ]
      - name:            25
        type:            dst
        dst-block:       9
        dst-successors:  [ 26 ]
      - name:            26
        type:            dst
        dst-block:       10
        dst-successors:  [ 27 ]
      - name:            27
        type:            dst
        dst-block:       11
        dst-successors:  [ 2 ]
      - name:            28
        type:            src
        src-block:       if.end15
        src-successors:  [ 29 ]
      - name:            29
        type:            src
        src-block:       if.end19
        src-successors:  [ 30 ]
      - name:            30
        type:            src
        src-block:       if.end23
        src-successors:  [ 31 ]
      - name:            31
        type:            src
        src-block:       if.end27
        src-successors:  [ 32 ]
      - name:            32
        type:            src
        src-block:       if.end31
        src-successors:  [ 33 ]
      - name:            33
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
      - name:            34
        type:            dst
        dst-block:       8
        dst-successors:  [ 35 ]
      - name:            35
        type:            dst
        dst-block:       9
        dst-successors:  [ 36 ]
      - name:            36
        type:            dst
        dst-block:       10
        dst-successors:  [ 37 ]
      - name:            37
        type:            dst
        dst-block:       11
        dst-successors:  [ 2 ]
      - name:            38
        type:            src
        src-block:       if.end19
        src-successors:  [ 39 ]
      - name:            39
        type:            src
        src-block:       if.end23
        src-successors:  [ 40 ]
      - name:            40
        type:            src
        src-block:       if.end27
        src-successors:  [ 41 ]
      - name:            41
        type:            src
        src-block:       if.end31
        src-successors:  [ 42 ]
      - name:            42
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
      - name:            43
        type:            dst
        dst-block:       9
        dst-successors:  [ 44 ]
      - name:            44
        type:            dst
        dst-block:       10
        dst-successors:  [ 45 ]
      - name:            45
        type:            dst
        dst-block:       11
        dst-successors:  [ 2 ]
      - name:            46
        type:            src
        src-block:       if.end23
        src-successors:  [ 47 ]
      - name:            47
        type:            src
        src-block:       if.end27
        src-successors:  [ 48 ]
      - name:            48
        type:            src
        src-block:       if.end31
        src-successors:  [ 49 ]
      - name:            49
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
      - name:            50
        type:            dst
        dst-block:       10
        dst-successors:  [ 51 ]
      - name:            51
        type:            dst
        dst-block:       11
        dst-successors:  [ 2 ]
      - name:            52
        type:            src
        src-block:       if.end27
        src-successors:  [ 53 ]
      - name:            53
        type:            src
        src-block:       if.end31
        src-successors:  [ 54 ]
      - name:            54
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
      - name:            55
        type:            dst
        dst-block:       11
        dst-successors:  [ 2 ]
      - name:            56
        type:            src
        src-block:       if.end31
        src-successors:  [ 57 ]
      - name:            57
        type:            src
        src-block:       for.inc
        src-successors:  [ 2 ]
    status:          valid
...
