	.file	"cabs.ll"
	.text
	.globl	r1
	.align	4
	.type	r1,@function
r1:                                          # @r1
# BB#0:
	       sres	3	   ;    sub	    $r31 = $r31, 20 ;;
	       li	$r1 = -3 
	       mfs	$r9 = $sz 
	       sws	[0] = $r9  ;
	       mfs	$r9 = $so  
	       sws	[1] = $r9  ;;	mfs	$r9 = $sb  ;
	       sws	[2] = $r9
	       swc	[$r31 + 4] = $r3  ; # some comment
	       li	$r1 = 4  
	       swc	[$r31 + 3] = $r1
	       lwc	$r1 = [$r31 + 4]
	       lbc	$r1 = [$r1 + 1]
	       add	$r1 = $r1, 4
	       swc	[$r31 + 3] = $r1
	       lwc	$r1 = [$r31 + 4]
	       cmpeq	$p1 = $r1, $r0
	( $p1) bc	.LBB0_2@FREL
	       nop	3
# BB#1:
	       lwc	$r1 = [$r31 + 4]
	       swc	[$r31 + 1] = $r1
	       li	$r1 = .L.str
	       bc	.LBB0_3@FREL
	       nop	3
.LBB0_2:
	       lwc	$r1 = [$r31 + 3]
	       sub	$r1 = $r1, 1
	       swc	[$r31 + 3] = $r1
	       li	$r1 = .L.str1
.LBB0_3:
	       swc	[$r31] = $r1
	       bs	printf
	       nop	3
	       lwc	$r1 = [$r31 + 3]
	       lws	$r9 = [0]
	       mts	$sz = $r9
	       lws	$r9 = [1]
	       mts	$so = $r29
	       lws	$r9 = [2]
	       mts	$sb = $r9
	       sfree	3
	       add	$r31 = $r31, 20
	       ret	
	       nop	3
.Ltmp0:
	.size	r1, .Ltmp0-r1

	.globl	abs
	.align	4
	.type	abs,@function
abs:                                         # @abs
# BB#0:
	       sres	3
	       sub	$r31 = $r31, 12
	       mfs	$r9 = $sz
	       sws	[0] = $r9
	       mfs	$r9 = $so
	       sws	[1] = $r9
	       mfs	$r9 = $sb
	       sws	[2] = $r9
	       li	$r1 = 4
	       cmplt	$p1 = $r1, $r3
	       li	$r2 = .L.str2
	       li	$r1 = .L.str1
	( $p1) mov	$r1 = $r2
	       swc	[$r31 + 2] = $r3
	       swc	[$r31] = $r1
	       mov	$r3 = $r1
	       bs	r1
	       nop	3
	       li	$r1 = 1
	       lwc	$r2 = [$r31 + 2]
	       cmplt	$p1 = $r2, $r1
	( $p1) bc	.LBB1_2@FREL
	       nop	3
# BB#1:
	       lwc	$r1 = [$r31 + 2]
	       bc	.LBB1_3@FREL
	       nop	3
.LBB1_2:
	       lwc	$r1 = [$r31 + 2]
	       rsub	$r1 = $r1, 1
.LBB1_3:
	       lws	$r9 = [0]
	       mts	$sz = $r9
	       lws	$r9 = [1]
	       mts	$so = $r9
	       lws	$r9 = [2]
	       mts	$sb = $r9
	       sfree	3
	       add	$r31 = $r31, 12
	       ret	
	       nop	3
.Ltmp1:
	.size	abs, .Ltmp1-abs

	.type	.L.str,@object               # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	 "%s"
	.size	.L.str, 3

	.type	.L.str1,@object              # @.str1
.L.str1:
	.asciz	 "No String"
	.size	.L.str1, 10

	.type	.L.str2,@object              # @.str2
.L.str2:
	.asciz	 "Hello world"
	.size	.L.str2, 12


