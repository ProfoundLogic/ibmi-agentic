     H* (c) 2010 StoneRiver, Inc. All rights reserved.
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE FS900RP - Load U&C Prof/Tech Component Percentages
     H*    DESCRIPTION: Read FTU1P and load FTUMP
     H*****************************************************************
     H*    CREATED: 09/17/10 C:257 PROJECT: MC345
     H*****************************************************************
     H* REVISIONS:
     H*
     H*****************************************************************
     f*  FILES:
     fftu1p     if   e             disk
     fftump     o    e             disk
     D*
     D psds          esds                  extname(szq1p)
     D*
     D                 ds
     Ddfdttm                   1     14  0
     Ddftime                   1      6  0
     Ddfdate                   7     14  0
     D*
     C*  Initialization Section
     C     *like         define    umco#         xxco#
     C     *like         define    umcpt4        xxcpt4
     C     *like         define    umefdt        xxefdt
     C     *like         define    u1perc        f1comp
     D*
     C*****************************************************************
     C* KLIST
     C*****************************************************************
     C*
     C     dtcymd        plist
     C                   parm                    srdti             8 0
     C                   parm      *BLANKS       srffmt            8
     C                   parm      '*CYMD'       srtfmt            8
     C                   parm                    srdto            10
     C                   parm                    srerr             1
     C*  Entry List
     C     *entry        plist
     C                   parm                    qqco#             3
     C                   parm                    qqtape           10
     C                   parm                    qqsel             1
     C                   parm                    qqpct             2
     C                   parm                    qqlabl           10
     C                   parm                    qqpgm            10
     C*
     C*****************************************************************
     C                   time                    dfdttm
     C*
     C                   read      pu101                                  60
     C                   dow       *in60 = *off
     C                   exsr      wrtftump
     C                   read      pu101                                  60
     C                   enddo
     C* ========
     C* End of Program
     C*
     C                   seton                                        lr
     C*================================================================
     C* write FTUMP file
     C*================================================================
     C     wrtftump      begsr
     C                   move      qqco#         umco#
     C                   eval      umcpt4 = u1code
     C                   eval      umfrcp = u1code
     C                   eval      umtocp = u1code
     C                   eval      umefdt = 10101
     C                   eval      f1comp = u1perc
     C                   eval      umod26   =  f1comp
     C                   eval      umod27   = (100 - f1comp)
     C                   eval      umstat   = 'A'
     C*
     C                   movel     q1user        umuser
     C                   movel     q1pgm         umpgm
     C                   movel     dftime        umtime
     C                   movel     dftime        umitim
     C                   z-add     dfdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         umdt
     C                   movel     srdto         umidt
     C                   movel     q1user        umiusr
     C                   movel     qqpgm         umipgm
     C                   write     pum01
     C                   endsr
     c*
