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
        successors:      [ 1, 2 ]
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
            opcode:          MOV
            size:            4
          - index:           6
            opcode:          MFS
            size:            4
          - index:           7
            opcode:          ISODD
            size:            4
          - index:           8
            opcode:          BR
            size:            4
            branch-type:     conditional
            branch-delay-slots: 2
            branch-targets:  [ 2 ]
          - index:           9
            opcode:          NOP
            size:            4
          - index:           10
            opcode:          SWS
            size:            4
            memmode:         store
            memtype:         stack
      - name:            1
        mapsto:          cond.false
        predecessors:    [ 0 ]
        successors:      [ 2 ]
        instructions:    
          - index:           0
            opcode:          MOV
            size:            4
          - index:           1
            opcode:          CALLND
            callees:         [ non_root ]
            size:            4
            branch-type:     call
          - index:           2
            opcode:          SENSi
            size:            4
            stack-cache-argument: 8
      - name:            2
        mapsto:          cond.end
        predecessors:    [ 1, 0 ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          LWS
            size:            4
            memmode:         load
            memtype:         stack
          - index:           1
            opcode:          NOP
            size:            4
          - index:           2
            opcode:          MTS
            size:            4
          - index:           3
            opcode:          LWS
            size:            4
            memmode:         load
            memtype:         stack
          - index:           4
            opcode:          NOP
            size:            4
          - index:           5
            opcode:          MTS
            size:            4
          - index:           6
            opcode:          LWS
            size:            4
            memmode:         load
            memtype:         stack
          - index:           7
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           8
            opcode:          NOP
            size:            4
          - index:           9
            opcode:          MTS
            size:            4
          - index:           10
            opcode:          SFREEi
            size:            4
            stack-cache-argument: 8
    subfunctions:    
      - name:            0
        blocks:          [ 0, 1, 2 ]
  - name:            0
    level:           machinecode
    mapsto:          non_root
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
            opcode:          LIl
            size:            8
          - index:           1
            opcode:          RET
            size:            4
            branch-type:     return
            branch-delay-slots: 3
          - index:           2
            opcode:          LWC
            size:            4
            memmode:         load
            memtype:         cache
          - index:           3
            opcode:          NOP
            size:            4
          - index:           4
            opcode:          ADDr
            size:            4
    subfunctions:    
      - name:            0
        blocks:          [ 0 ]
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
        successors:      [ cond.false, cond.end ]
        instructions:    
          - index:           0
            opcode:          and
          - index:           1
            opcode:          icmp
          - index:           2
            opcode:          br
      - name:            cond.false
        predecessors:    [ entry ]
        successors:      [ cond.end ]
        instructions:    
          - index:           0
            opcode:          call
            callees:         [ non_root ]
          - index:           1
            opcode:          br
      - name:            cond.end
        predecessors:    [ entry, cond.false ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          ret
  - name:            non_root
    level:           bitcode
    hash:            0
    blocks:          
      - name:            entry
        predecessors:    [  ]
        successors:      [  ]
        instructions:    
          - index:           0
            opcode:          load
            memmode:         load
          - index:           1
            opcode:          add
          - index:           2
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
        src-successors:  [ 2, 3 ]
        dst-successors:  [ 2, 3 ]
      - name:            1
        type:            exit
      - name:            2
        type:            progress
        src-block:       cond.end
        dst-block:       2
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
      - name:            3
        type:            progress
        src-block:       cond.false
        dst-block:       1
        src-successors:  [ 2 ]
        dst-successors:  [ 2 ]
    status:          valid
  - src:             
      function:        non_root
      level:           bitcode
    dst:             
      function:        0
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
...
