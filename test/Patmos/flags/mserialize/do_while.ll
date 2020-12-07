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
            opcode:          LIl
            size:            8
          - index:           2
            opcode:          MFS
            size:            4
      - name:            1
        mapsto:          do.body
        predecessors:    [ 0, 1 ]
        successors:      [ 1, 2 ]
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
            opcode:          CMPLT
            size:            4
          - index:           4
            opcode:          BRND
            size:            4
            branch-type:     conditional
            branch-targets:  [ 1 ]
      - name:            2
        mapsto:          do.end
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
        successors:      [ do.body ]
        instructions:    
          - index:           0
            opcode:          br
      - name:            do.body
        predecessors:    [ do.cond, entry ]
        successors:      [ do.cond ]
        loops:           [ do.body ]
        instructions:    
          - index:           0
            opcode:          phi
          - index:           1
            opcode:          load
            memmode:         load
          - index:           2
            opcode:          add
          - index:           3
            opcode:          br
      - name:            do.cond
        predecessors:    [ do.body ]
        successors:      [ do.body, do.end ]
        loops:           [ do.body ]
        instructions:    
          - index:           0
            opcode:          icmp
          - index:           1
            opcode:          br
      - name:            do.end
        predecessors:    [ do.cond ]
        successors:      [  ]
        instructions:    
          - index:           0
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
        src-block:       do.body
        dst-block:       1
        src-successors:  [ 3 ]
        dst-successors:  [ 2, 4 ]
      - name:            3
        type:            src
        src-block:       do.cond
        src-successors:  [ 2, 4 ]
      - name:            4
        type:            progress
        src-block:       do.end
        dst-block:       2
        src-successors:  [ 1 ]
        dst-successors:  [ 1 ]
    status:          valid
...
