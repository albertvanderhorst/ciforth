
.org 0x7c00		# This means we'll have to trim the
			#    binary with dd or something. Well worth it.
			#    After   as   and   objcopy -O binary   do
			#    dd < a.out > whatever bs=512 skip=62

BITS 16
.global _start
_start:

cli	;

movw $gdt_entries , %si	# copy initial GDT descriptors to  x2000
movw $0x2008 , %di		#	skip and thus allocate entry 0
mov %di , %bx
add $0x100 , %bx
for_128_duals:			# move 256 bytes
	mov  (%si) , %ax
	mov  %ax, %es:(%di)
	add $2 , %di
	add $2 , %si
	cmp %bx , %di
jnz for_128_duals
				# 	assuming %es=0 and ss makes sense.
				#	My P166 BIOS allows this. YMMV.

lgdt initial_gdtr		# This is one reason for the .org 0x7c00

mov %cr0 , %ecx
inc %cx
mov %ecx , %cr0		# protection is on, but we need to get into
				# 32 bit segments. You have to branch to
				# a new cs, mov won't do cs.

				# hand assembled jmp sd1:cs32
DB   0x66                    # we're still IN 16 bit
DB   0xea
.long   cs32
DW   0x08                    # 8 is GDT index 1, our code seg. selector

cs32:

BITS 32                 # since we just jumped to a 32 bit segment

mov $0x10 , %eax		# selector 0x10 is index 2, our 32 bit data
movw %ax , %ds
movw %ax , %ss			# This might help with the 32/16/32 stunts.

mov $0x9000 , %esp		# first 64k of second meg is our stack

#
## BUILD Interrupt Descriptor Table at 0x1000 physical
#	256 interrupt gate descriptors that call sequential IIT
#	calls, which all call INTCALLER.

movw $0x7003 , %eax
movl $0x1000 , %edi
per_IDT_descriptor:			# Our first (example) entry is...
	movw %eax , (%edi)		# 03 70       starting at 0x1000
	inc %edi			# offset
	inc %edi
	movw $0x08 , (%edi)
	inc %edi			# 03 70 	08 00
	inc %edi			# 		pmode code sel.
	movl $0x00008e00 , (%edi)
	inc %edi			# 03 70 20 00 	00 8e 00 00
	inc %edi			# 		present    intrrgate
	inc %edi
	inc %edi
	add $8 , %eax
	cmp $0x7803, %eax
jnz per_IDT_descriptor

lidt initial_idtr

#
## BUILD Int Indirection Table at 0x7000 <-> 0x7800. This is a bunch of
#	calls that INTCALLER can pop the return address of to find out
#	what int is happening.

mov	$0xff , %ebx
mov 	$0x7000 , %edi
mov	$(INTCALLER - 0x7008)  , %eax		# first relative offset
						# byte following
while_B:
		# dest is cursor into IIT
		# eax is current call offset
		# ebx is an entry counter, and is extraneous

	movl	$0xe8909090 , (%edi)	# e8 opcode for CALL rel32
					# prefixed with NOPs. Note endianism.
	add $4 ,  %edi
	movl	%eax , (%edi)		# append 4 byte relative offset
	add $4 ,  %edi
	subl	$8, %eax			# relative offset from next IIT call to
					# INTCALLER will be 8 less.
	dec %ebx
jnz while_B

sti	;

HANG: nop  ;

	mov  0xb8060 , %eax
	inc %eax
	inc %eax
	mov %eax , 0xb8060
	int $0x40

jmp HANG ###########
#################################

INTCALLER:		# System code, so to speak.

			# we are in the system 32-bit segments

mov %eax , 0x6300
pop %eax			# gotta pop the I.I.T. return value to calculate
			#   which int we are.

 mov  0xb8060 + 320 , %eax
 inc %eax
 inc %eax
 mov %eax , 0xb8060 +320

DB   0xea                    # far jmp to a suitable-for-real-mode cs
.long	small_code_segment
DW   0x20
small_code_segment:		# we are a 16-bit machine with no useful
				# 	segments.

BITS 16

movl $0x18 , %eax			# upgrade to an 8086

movw %eax , %ds
movw %eax , %ss

mov %cr0 , %ecx
dec %cx
mov %ecx , %cr0		# turn off protection, losing the segments
				# 	again.

DB   0xea
DW   rereal
DW   0x0                     # far jmp to an 8086 cs, NOT a dscriptor

rereal:

mov $0xb800 , %ax		# real mode demo code. This, I think, could be
mov %ax , %ds			#   an 8086 int caller, which is
mov 0x6400 , %ax
sub $1, %ax			#   Left as an Excercize to the Reader.
mov %ax , 1620			#   HINT:  pushf
mov %ax , 0x6400

mov %cr0 , %ecx
inc %ecx
mov %ecx , %cr0

DB   0xea
DW   recs32
DW   0x08                    # 8 is GDT index 1, our code seg. selector

recs32:

BITS 32

movw $0x10 , %eax

mov %eax , %ds
mov %eax , %ss

 mov  0xb8060 + 640 , %eax
 inc %eax
 inc %eax
 mov %eax , 0xb8060 +640

iret

############### initialized DATA allocations      ####################

gdt_entries:
	# The GDT copy routine skip-allocates index 0.
	# We initialize from index 1 on. Index 1 = selector 8.
	# This template is for typical segments, not gates and such.

        DW   0xFFFF  # low dual of limit
	DW   0       # low dual of base
	DB   0       # bits 16<->23 of base

				# The DFbyte, see also the DFnybble
				#
				# bit	on=	description	  bit of 64
				# 0	0x1	accessed		40
				# 1	0x2	read/write		41
				# 2	0x4	expandup/conform	42
				# 3	0x8	executable?		43
				# 4	0x10	typical			44
				# 5	0x20	DPL lo			45
				# 6	0x40	DPL hi			46
				# 7	0x80	present			47
	DB   0x9A    # = present typical executable readable

				# bit	on=     description	  bit of 64
                                # 0	0x1    	limit hi nybble		48
                                # 1     0x2     	"		49
                                # 2     0x4     	"		50
                                # 3     0x8     	"		51
				# The DFnybble(TM)
				# 4     0x10    AVL			52
                                # 5     0x20    0			53
                                # 6     0x40    Big/D-efault/X		54
				# D-bit true = 32 bit addy/operands in pmode
                                # 7     0x80    4k Granularity		55
	DB   0xCF    # = *4k Big   f-nybble
	DB   0x00    # hi byte of base

# index 2, 32 bit data, selector val = 0x10, 4 gig limit, 0 base
        DW   0xFFFF
	DW   0
	DB   0x00
	DB   0x92    # present typical writable
	DB   0xcf    # *4k  BIG  f-nybble
	DB   0x00

# index 3, 16 bit data, selector val = 0x18, 64k limit, 0 base
        DW   0xFFFF  # limit
        DW   0
        DB   0
        DB   0x92    # present typical writable
        DB   0x0     # (bytegrain) 0-max-nybble-of-limit
        DB   0

# index 4, 16 bit code, selector val = 0x20, 64k lim/gran, 0000 base
# this is base 0 real-ish, for INTCALLER , ffff limit

        DW   0xFFFF  # limit
        DW   0       # this is physical
	DB   0x0     #     this OR previous dual = 0000
	DB   0x9a    # present typical executable readable
	DB   0x0     # (smallgrain) (notBIG) 0-max-nybble-of-limit
        DB   0

#,.....................................................

## 6 byte limits/pointers for IDTR & GDTR

initial_idtr:
	DW  0x800            # 2k, 256 * 8
	.long  0x1000		# gdt physical address

initial_gdtr:
	DW  0x400            # gdt limit
	.long  0x2000		# gdt physical address

.org 0x7dfe
DW  0xAA55   # Claim to be a PEEEEEEE.CEE bootsector.

STAGE_II:
# get a floppy load on, put your OS here, and jmp here.

# Copyright 2000 Rick Hohensee
# References/keys: Intel 386 manuals, John Fine, GNU as.info, gazos
#	Ralf Brown's, Linux
# Janet_Reno.s
