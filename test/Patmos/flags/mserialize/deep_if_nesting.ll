; RUN: %check-pml
; END.
;//////////////////////////////////////////////////////////////////////////////////////////////////
---
format:          pml-0.1
triple:          patmos-unknown-unknown-elf
machine-functions: 
  - name:            1
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
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          SRESi
            size:            4
            stack-cache-argument: 8
          - index:           1
            opcode:          MFS
            size:            4
          - index:           2
            opcode:          SWS
            size:            4
            memmode:         store
            memtype:         stack
          - index:           3
            opcode:          MFS
            size:            4
          - index:           4
            opcode:          SWS
            size:            4
            memmode:         store
            memtype:         stack
          - index:           5
            opcode:          LIl
            size:            8
          - index:           6
            opcode:          SWC
            size:            4
            memmode:         store
            memtype:         cache
          - index:           7
            opcode:          CALLND
            callees:         [ init_func ]
            size:            4
            branch-type:     call
          - index:           8
            opcode:          SENSi
            size:            4
            stack-cache-argument: 8
          - index:           9
            opcode:          LWS
            size:            4
            memmode:         load
            memtype:         stack
          - index:           10
            opcode:          NOP
            size:            4
          - index:           11
            opcode:          MTS
            size:            4
          - index:           12
            opcode:          LWS
            size:            4
            memmode:         load
            memtype:         stack
          - index:           13
            opcode:          NOP
            size:            4
          - index:           14
            opcode:          MTS
            size:            4
          - index:           15
            opcode:          NOP
            size:            4
          - index:           16
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           17
            opcode:          NOP
            size:            4
          - index:           18
            opcode:          NOP
            size:            4
          - index:           19
            opcode:          SFREEi
            size:            4
            stack-cache-argument: 8
    subfunctions:    
      - name:            0
        blocks:          [ 0 ]
  - name:            0
    level:           machinecode
    mapsto:          init_func
    hash:            0
    blocks:          
      - name:            0
        mapsto:          entry
        predecessors:    [  ]
        successors:      [ 1, 9 ]
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
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           4
            opcode:          LIi
            size:            4
          - index:           5
            opcode:          CMPLT
            size:            4
          - index:           6
            opcode:          BRCFND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 9 ]
      - name:            1
        mapsto:          if.then
        predecessors:    [ 0 ]
        successors:      [ 2, 10 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LIi
            size:            4
          - index:           2
            opcode:          CMPLT
            size:            4
          - index:           3
            opcode:          BRCFND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 10 ]
      - name:            2
        mapsto:          if.then2
        predecessors:    [ 1 ]
        successors:      [ 3, 11 ]
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
            opcode:          LIi
            size:            4
          - index:           3
            opcode:          CMPLT
            size:            4
          - index:           4
            opcode:          BRCFND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 11 ]
      - name:            3
        mapsto:          if.then4
        predecessors:    [ 2 ]
        successors:      [ 4, 12 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LIi
            size:            4
          - index:           2
            opcode:          CMPLT
            size:            4
          - index:           3
            opcode:          BRCFND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 12 ]
      - name:            4
        mapsto:          if.then6
        predecessors:    [ 3 ]
        successors:      [ 5, 13 ]
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
            opcode:          LIi
            size:            4
          - index:           3
            opcode:          CMPLT
            size:            4
          - index:           4
            opcode:          BRCFND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 13 ]
          - index:           5
            opcode:          BRCFNDu
            size:            4
            branch-type:     unconditional
            branch-targets:  [ 5 ]
      - name:            5
        mapsto:          if.then8
        predecessors:    [ 4 ]
        successors:      [ 6, 7 ]
        instructions:    
          - index:           0
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           1
            opcode:          LIi
            size:            4
          - index:           2
            opcode:          CMPLT
            size:            4
          - index:           3
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 7 ]
      - name:            6
        mapsto:          if.then10
        predecessors:    [ 5 ]
        successors:      [ 8 ]
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
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          CMPLT
            size:            4
          - index:           4
            opcode:          ADDi
            size:            4
          - index:           5
            opcode:          BRu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 2
            branch-targets:  [ 8 ]
          - index:           6
            opcode:          SUBi
            size:            4
          - index:           7
            opcode:          SUBi
            size:            4
      - name:            7
        mapsto:          if.else14
        predecessors:    [ 5 ]
        successors:      [ 8 ]
        instructions:    
          - index:           0
            opcode:          ADDi
            size:            4
      - name:            8
        mapsto:          '(null)'
        predecessors:    [ 7, 6 ]
        successors:      [ 14 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 14 ]
          - index:           1
            opcode:          SUBi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            9
        mapsto:          if.else34
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
      - name:            10
        mapsto:          if.else30
        predecessors:    [ 1 ]
        successors:      [ 17 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 17 ]
          - index:           1
            opcode:          ADDi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            11
        mapsto:          if.else26
        predecessors:    [ 2 ]
        successors:      [ 16 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 16 ]
          - index:           1
            opcode:          ADDi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            12
        mapsto:          if.else22
        predecessors:    [ 3 ]
        successors:      [ 15 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 15 ]
          - index:           1
            opcode:          ADDi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            13
        mapsto:          if.else18
        predecessors:    [ 4 ]
        successors:      [ 14 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 14 ]
          - index:           1
            opcode:          ADDi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            14
        mapsto:          '(null)'
        predecessors:    [ 8, 13 ]
        successors:      [ 15 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 15 ]
          - index:           1
            opcode:          SUBi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            15
        mapsto:          '(null)'
        predecessors:    [ 14, 12 ]
        successors:      [ 16 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 16 ]
          - index:           1
            opcode:          SUBi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            16
        mapsto:          '(null)'
        predecessors:    [ 15, 11 ]
        successors:      [ 17 ]
        instructions:    
          - index:           0
            opcode:          BRCFu
            size:            4
            branch-type:     unconditional
            branch-delay-slots: 3
            branch-targets:  [ 17 ]
          - index:           1
            opcode:          SUBi
            size:            4
          - index:           2
            opcode:          NOP
            size:            4
          - index:           3
            opcode:          NOP
            size:            4
      - name:            17
        mapsto:          '(null)'
        predecessors:    [ 16, 10 ]
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
            opcode:          SUBi
            size:            4
          - index:           3
            opcode:          MTS
            size:            4
    subfunctions:    
      - name:            0
        blocks:          [ 0, 1, 2, 3, 4 ]
      - name:            5
        blocks:          [ 5, 6, 7, 8 ]
      - name:            9
        blocks:          [ 9 ]
      - name:            10
        blocks:          [ 10 ]
      - name:            11
        blocks:          [ 11 ]
      - name:            12
        blocks:          [ 12 ]
      - name:            13
        blocks:          [ 13 ]
      - name:            14
        blocks:          [ 14 ]
      - name:            15
        blocks:          [ 15 ]
      - name:            16
        blocks:          [ 16 ]
      - name:            17
        blocks:          [ 17 ]
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
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          store
            memmode:         store
          - index:           1
            opcode:          call
            callees:         [ init_func ]
          - index:           2
            opcode:          ret
  - name:            init_func
    level:           bitcode
    hash:            0
    blocks:          
      - name:            entry
        predecessors:    [  ]
        successors:      [ if.then, if.else34 ]
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
      - name:            if.then
        predecessors:    [ entry ]
        successors:      [ if.then2, if.else30 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then2
        predecessors:    [ if.then ]
        successors:      [ if.then4, if.else26 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then4
        predecessors:    [ if.then2 ]
        successors:      [ if.then6, if.else22 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then6
        predecessors:    [ if.then4 ]
        successors:      [ if.then8, if.else18 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then8
        predecessors:    [ if.then6 ]
        successors:      [ if.then10, if.else14 ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then10
        predecessors:    [ if.then8 ]
        successors:      [ if.then12, if.else ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            if.then12
        predecessors:    [ if.then10 ]
        successors:      [ if.end ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.else
        predecessors:    [ if.then10 ]
        successors:      [ if.end ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end
        predecessors:    [ if.else, if.then12 ]
        successors:      [ if.end16 ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else14
        predecessors:    [ if.then8 ]
        successors:      [ if.end16 ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end16
        predecessors:    [ if.else14, if.end ]
        successors:      [ if.end20 ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else18
        predecessors:    [ if.then6 ]
        successors:      [ if.end20 ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end20
        predecessors:    [ if.else18, if.end16 ]
        successors:      [ if.end24 ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else22
        predecessors:    [ if.then4 ]
        successors:      [ if.end24 ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end24
        predecessors:    [ if.else22, if.end20 ]
        successors:      [ if.end28 ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else26
        predecessors:    [ if.then2 ]
        successors:      [ if.end28 ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end28
        predecessors:    [ if.else26, if.end24 ]
        successors:      [ if.end32 ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else30
        predecessors:    [ if.then ]
        successors:      [ if.end32 ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end32
        predecessors:    [ if.else30, if.end28 ]
        successors:      [ if.end36 ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          add
          - index:           2
            opcode:          br
      - name:            if.else34
        predecessors:    [ entry ]
        successors:      [ if.end36 ]
        instructions:    
          - index:           0
            opcode:          add
          - index:           1
            opcode:          br
      - name:            if.end36
        predecessors:    [ if.else34, if.end32 ]
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
      function:        1
      level:           machinecode
    nodes:           
      - name:            0
        type:            entry
        src-block:       entry
        dst-block:       0
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            1
        type:            exit
    status:          valid
  - src:             
      function:        init_func
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
        src-block:       if.else34
        dst-block:       9
        src-successors:  [ 63 ]
        dst-successors:  [ 1 ]
      - name:            3
        type:            progress
        src-block:       if.then
        dst-block:       1
        src-successors:  [ 4, 5 ]
        dst-successors:  [ 4, 5 ]
      - name:            4
        type:            progress
        src-block:       if.else30
        dst-block:       10
        src-successors:  [ 60 ]
        dst-successors:  [ 62 ]
      - name:            5
        type:            progress
        src-block:       if.then2
        dst-block:       2
        src-successors:  [ 6, 7 ]
        dst-successors:  [ 6, 7 ]
      - name:            6
        type:            progress
        src-block:       if.else26
        dst-block:       11
        src-successors:  [ 55 ]
        dst-successors:  [ 58 ]
      - name:            7
        type:            progress
        src-block:       if.then4
        dst-block:       3
        src-successors:  [ 8, 9 ]
        dst-successors:  [ 8, 9 ]
      - name:            8
        type:            progress
        src-block:       if.else22
        dst-block:       12
        src-successors:  [ 48 ]
        dst-successors:  [ 52 ]
      - name:            9
        type:            progress
        src-block:       if.then6
        dst-block:       4
        src-successors:  [ 10, 11 ]
        dst-successors:  [ 10, 11 ]
      - name:            10
        type:            progress
        src-block:       if.else18
        dst-block:       13
        src-successors:  [ 39 ]
        dst-successors:  [ 44 ]
      - name:            11
        type:            progress
        src-block:       if.then8
        dst-block:       5
        src-successors:  [ 12, 13 ]
        dst-successors:  [ 12, 13 ]
      - name:            12
        type:            progress
        src-block:       if.else14
        dst-block:       7
        src-successors:  [ 28 ]
        dst-successors:  [ 34 ]
      - name:            13
        type:            progress
        src-block:       if.then10
        dst-block:       6
        src-successors:  [ 14, 15 ]
        dst-successors:  [ 23 ]
      - name:            14
        type:            src
        src-block:       if.then12
        src-successors:  [ 16 ]
      - name:            15
        type:            src
        src-block:       if.else
        src-successors:  [ 16 ]
      - name:            16
        type:            src
        src-block:       if.end
        src-successors:  [ 17 ]
      - name:            17
        type:            src
        src-block:       if.end16
        src-successors:  [ 18 ]
      - name:            18
        type:            src
        src-block:       if.end20
        src-successors:  [ 19 ]
      - name:            19
        type:            src
        src-block:       if.end24
        src-successors:  [ 20 ]
      - name:            20
        type:            src
        src-block:       if.end28
        src-successors:  [ 21 ]
      - name:            21
        type:            src
        src-block:       if.end32
        src-successors:  [ 22 ]
      - name:            22
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
      - name:            23
        type:            dst
        dst-block:       8
        dst-successors:  [ 24 ]
      - name:            24
        type:            dst
        dst-block:       14
        dst-successors:  [ 25 ]
      - name:            25
        type:            dst
        dst-block:       15
        dst-successors:  [ 26 ]
      - name:            26
        type:            dst
        dst-block:       16
        dst-successors:  [ 27 ]
      - name:            27
        type:            dst
        dst-block:       17
        dst-successors:  [ 1 ]
      - name:            28
        type:            src
        src-block:       if.end16
        src-successors:  [ 29 ]
      - name:            29
        type:            src
        src-block:       if.end20
        src-successors:  [ 30 ]
      - name:            30
        type:            src
        src-block:       if.end24
        src-successors:  [ 31 ]
      - name:            31
        type:            src
        src-block:       if.end28
        src-successors:  [ 32 ]
      - name:            32
        type:            src
        src-block:       if.end32
        src-successors:  [ 33 ]
      - name:            33
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
      - name:            34
        type:            dst
        dst-block:       8
        dst-successors:  [ 35 ]
      - name:            35
        type:            dst
        dst-block:       14
        dst-successors:  [ 36 ]
      - name:            36
        type:            dst
        dst-block:       15
        dst-successors:  [ 37 ]
      - name:            37
        type:            dst
        dst-block:       16
        dst-successors:  [ 38 ]
      - name:            38
        type:            dst
        dst-block:       17
        dst-successors:  [ 1 ]
      - name:            39
        type:            src
        src-block:       if.end20
        src-successors:  [ 40 ]
      - name:            40
        type:            src
        src-block:       if.end24
        src-successors:  [ 41 ]
      - name:            41
        type:            src
        src-block:       if.end28
        src-successors:  [ 42 ]
      - name:            42
        type:            src
        src-block:       if.end32
        src-successors:  [ 43 ]
      - name:            43
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
      - name:            44
        type:            dst
        dst-block:       14
        dst-successors:  [ 45 ]
      - name:            45
        type:            dst
        dst-block:       15
        dst-successors:  [ 46 ]
      - name:            46
        type:            dst
        dst-block:       16
        dst-successors:  [ 47 ]
      - name:            47
        type:            dst
        dst-block:       17
        dst-successors:  [ 1 ]
      - name:            48
        type:            src
        src-block:       if.end24
        src-successors:  [ 49 ]
      - name:            49
        type:            src
        src-block:       if.end28
        src-successors:  [ 50 ]
      - name:            50
        type:            src
        src-block:       if.end32
        src-successors:  [ 51 ]
      - name:            51
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
      - name:            52
        type:            dst
        dst-block:       15
        dst-successors:  [ 53 ]
      - name:            53
        type:            dst
        dst-block:       16
        dst-successors:  [ 54 ]
      - name:            54
        type:            dst
        dst-block:       17
        dst-successors:  [ 1 ]
      - name:            55
        type:            src
        src-block:       if.end28
        src-successors:  [ 56 ]
      - name:            56
        type:            src
        src-block:       if.end32
        src-successors:  [ 57 ]
      - name:            57
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
      - name:            58
        type:            dst
        dst-block:       16
        dst-successors:  [ 59 ]
      - name:            59
        type:            dst
        dst-block:       17
        dst-successors:  [ 1 ]
      - name:            60
        type:            src
        src-block:       if.end32
        src-successors:  [ 61 ]
      - name:            61
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
      - name:            62
        type:            dst
        dst-block:       17
        dst-successors:  [ 1 ]
      - name:            63
        type:            src
        src-block:       if.end36
        src-successors:  [ 1 ]
    status:          valid
...
