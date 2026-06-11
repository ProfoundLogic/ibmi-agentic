     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*====================================================================
     H/title LODSYSICD9 - Load ICD9 Information to System Files from MC
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     Fsmcol     if   e           k disk    rename(pco01:pco00)
     F*
     Fftgil     if   e           k disk    rename(pgi01:fgi00)
     Fwtgil     if a e           k disk    rename(pgi01:wgi00) usropn
     F*
     Fftgtl     if   e           k disk    rename(pgt01:fgt00)
     Fwtgtl     if a e           k disk    rename(pgt01:wgt00) usropn
     F*
     Ffmdil     if   e           k disk    rename(pdi01:fdi00)
     Fsmdil     if a e           k disk    rename(pdi01:sdi00)
     F*
     Ffadal     if   e           k disk    rename(pda01:fda00)
     Fsadal     if a e           k disk    rename(pda01:sda00)
     F*
     D#ofStates        c                   53
     D state           s              2    dim(#ofStates) ctdata perrcd(1)      Options, Function
     D*
     D xxCo#           s                   like(COCO#)
     D xxTBID          s                   like(GITBID)
     D onlyTBID        s              1n
     D WCMS            s              1n   inz('1')
     D st              s              3  0
     D ##Pgm           s             10
     D ##Exists        s              1n
     D ##Obj           s             10
     D ##ObjType       s              7
     D ##Error         s              1n
     C*
     C     keyTable      klist
     C                   kfld                    xxCo#
     C                   kfld                    xxTBID
     C*
     C     key1Table     klist
     C                   kfld                    xxCo#
     C                   kfld                    xxTBID
     C                   kfld                    GTKEY
     C*
     C     keyDIL        klist
     C                   kfld                    DIICD9
     C                   kfld                    DILOST
     C*
     C     keyDAL        klist
     C                   kfld                    DACO#
     C                   kfld                    DASUB#
     C                   kfld                    DAFND
     C                   kfld                    DAFYR
     C                   kfld                    DACASE
     C                   kfld                    DASEQ#
     C                   kfld                    DAICD9
     C*
     C                   call      'SRCHKPGM'
     C                   parm      'F2375R01'    ##Pgm
     C                   parm                    ##Exists
     C                   if        ##Exists
     C                   eval      onlyTBID = *on
     C                   endif
     C*
     C                   call      'SRCHKPGM'
     C                   parm      'F2380R01'    ##Pgm
     C                   parm                    ##Exists
     C                   if        ##Exists
     C                   eval      onlyTBID = *on
     C                   endif
     C*
     C                   call      'SRCHKOBJ'
     C                   parm      'WTGIP'       ##Obj
     C                   parm      '*FILE'       ##ObjType
     C                   parm                    ##Error
     C                   if        ##Error
     C                   eval      WCMS = *off
     C                   endif
     C*
     C                   dou       %eof(smcol)
     C                   read      smcol
     C                   if        not %eof
     C                   exsr      ProcessCo#
     C                   endif
     C                   enddo
     C*
     C                   eval      *inlr = *on
     C*
     C*====================================================================
     C* Process each company
     C*====================================================================
     C     ProcessCo#    begsr
     C*
     C                   eval      xxCo# = COCO#
     C*
     C                   if        WCMS
     C                   exsr      ProcessTBID
     C                   endif
     C*
     C                   if        not onlyTBID
     C                   exsr      ProcessICD9
     C                   endif
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process the generic table ID file
     C*====================================================================
     C     Proceswgi     begsr
     C*
     C                   open      WTGIL
     C                   open      WTGTL
     C*
     C     keyTable      chain     ftgil
     C                   if        %found
     C     keyTable      chain     wtgil
     C                   if        not %found
     C                   write     wgi00
     C                   endif
     C                   exsr      Proceswgt
     C                   endif
     C*
     C                   close     WTGIL
     C                   close     WTGTL
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process the generic table information file
     C*====================================================================
     C     Proceswgt     begsr
     C*
     C     keyTable      setll     ftgtl
     C                   dou       %eof(ftgtl)
     C     keyTable      reade     ftgtl
     C                   if        not %eof(ftgtl)
     C     key1Table     chain     wtgtl
     C                   if        not %found
     C                   write     wgt00
     C                   endif
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process the ICD9 files
     C*====================================================================
     C     ProcessICD9   begsr
     C*
     C     *loval        setll     fmdil
     C                   dou       %eof(fmdil)
     C                   read      fmdil
     C                   if        not %eof(fmdil)
     C     keyDIL        chain     smdil
     C                   if        not %found
     C                   write     sdi00
     C                   endif
     C                   endif
     C                   enddo
     C*
     C     xxCo#         setll     fadal
     C                   dou       %eof(fadal)
     C     xxCo#         reade     fadal
     C                   if        not %eof(fadal)
     C     keyDAL        chain     sadal
     C                   if        not %found
     C                   write     sda00
     C                   endif
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Process the generic table files
     C*====================================================================
     C     ProcessTBID   begsr
     C*
     C                   eval      xxTBID = 'ILOC'
     C                   exsr      Proceswgi
     C                   eval      xxTBID = 'ISIT'
     C                   exsr      Proceswgi
     C                   eval      xxTBID = 'ISTS'
     C                   exsr      Proceswgi
     C*
     C     1             do        #ofStates     st
     C                   eval      xxTBID = state(st) + 'IL'
     C                   exsr      Proceswgi
     C                   eval      xxTBID = state(st) + 'IS'
     C                   exsr      Proceswgi
     C                   enddo
     C*
     C                   endsr
** States
ZZ
AL
AK
AZ
AR
CA
CO
CT
DE
FL
GA
HI
ID
IL
IN
IA
KS
KY
LA
ME
MD
MA
MI
MN
MS
MO
MT
NE
NV
NH
NJ
NM
NY
NC
ND
OH
OK
OR
PA
RI
SC
SD
TN
TX
UT
VT
VA
WA
WV
WI
WY
PR
DC
