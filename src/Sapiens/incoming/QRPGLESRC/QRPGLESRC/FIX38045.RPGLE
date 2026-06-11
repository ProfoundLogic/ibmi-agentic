     H* (C) Copyright Progressive Data Solutions, 2003
     H/TITLE FIX38045                                                                       *
     H**************************************************************************************
     H*  Description:  Add values to all proc code modifier table                          *
     H*                                                                                    *
     H*  Created: 07/18/03   Programmer C:79    Support:MC010 38045                        *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FSTSTL     IF   E             DISK
     FFTABL     UF A E           K DISK    usropn
     FSMUPL     UF A E           K DISK

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     D length          s             15  5
     D qca             s             56    dim(2) ctdata perrcd(1)
     D                 DS
     D qqcmd                         56
     D  qqst                          2    overlay(qqcmd:38)
     D pmod            s             10    dim(33) perrcd(1) ctdata
     D pmoddes         s             40    dim(33) alt(pmod)
     D kkusr           s                   like(upusr)
     D kkkey           s                   like(pmkey)
     D x               s              3  0

     DPSDS           ESDS                  Extname(SZQ1P)


     C     pmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey

     C     upky          klist
     C                   kfld                    kkusr

     C     *like         define    upco#         kkco#

     C*
     C*----------------------------------------------------------------------
     C*  Read through STSTP and add records to each state file
     C*----------------------------------------------------------------------
     C                   eval      kkusr  = q1user
     C*
     C     upky          chain     pup01
     C                   eval      kkco# = upco#
     C*
     C                   read      pst01                                  99
     C                   dow       *in99 = *off
     C*
     C* Override to the state file
     C*
     C                   eval      length = 56
     C                   movel(p)  qca(1)        qqcmd
     C                   movel(p)  stkey         qqst
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C*
     C                   open      ftabl
     C*
     C* Read through table of new modifiers
     C*
     C                   eval      x = 1
     C*
     C                   dow       x <= 33
     C*
     C                   eval      kkkey = pmod(x)
     C*
     C     pmky          chain     ppm01                              99
     C*
     C                   if        *in99 = *on
     C                   exsr      writepmod
     C                   else
     C                   exsr      updatepmod
     C                   endif
     C*
     C                   eval      x = x + 1
     C*
     C                   enddo                                                  *in60=*off
     C*
     C* Close file and delete override
     C*
     C                   close     ftabl
     C*
     C                   eval      length = 56
     C                   movel(p)  qca(2)        qqcmd
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C*
     C                   read      pst01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   eval      *inlr=*on
     C*
     C                   Return
     C*
     C**********************************************************************
     C     updatepmod    begsr
     C**********************************************************************
     C*
     C                   eval      pmdesc= pmoddes(x)
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      pmdt
     C                   eval      pmpgm=q1pgm
     C                   eval      pmuser=q1user
     C                   move      sys_time      pmtime
     C*
     C                   update    ppm01
     C*
     C                   endsr
     C*
     C**********************************************************************
     C     writepmod     begsr
     C**********************************************************************
     C*
     C                   clear                   ppm01
     C*
     C                   eval      pmkey = pmod(x)
     C                   eval      pmdesc= pmoddes(x)
     C                   eval      pmstat= 'A'
     C                   eval      pmco# = kkco#
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C     *cymd         move      sys_date      pmdt
     C                   eval      pmpgm=q1pgm
     C                   eval      pmuser=q1user
     C                   move      sys_time      pmtime
     C*
     C     *cymd         move      sys_date      pmidt
     C                   eval      pmiusr=q1user
     C*
     C                   write     ppm01
     C*
     C                   endsr
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
DLTOVR FILE(FTABL)
**  (PMOD)
79        UNRELATED PROCEDURE BY THE SAME PROVIDER
E1        UPPER LEFT, EYELID
E2        LOWER LEFT, EYELID
E3        UPPER RIGHT, EYELID
E4        LOWER RIGHT, EYELID
FA        LEFT HAND, THUMB
F1        LEFT HAND, SECOND DIGIT
F2        LEFT HAND, THIRD DIGIT
F3        LEFT HAND, FOURTH DIGIT
F4        LEFT HAND, FIFTH DIGIT
F5        RIGHT HAND, THUMB
F6        RIGHT HAND, SECOND DIGIT
F7        RIGHT HAND, THIRD DIGIT
F8        RIGHT HAND, FOURTH DIGIT
F9        RIGHT HAND, FIFTH DIGIT
TA        LEFT FOOT, GREAT TOE
T1        LEFT FOOT, SECOND DIGIT
T2        LEFT FOOT, THIRD DIGIT
T3        LEFT FOOT, FOURTH DIGIT
T4        LEFT FOOT, FIFTH DIGIT
T5        RIGHT FOOT, GREAT TOE
T6        RIGHT FOOT, SECOND DIGIT
T7        RIGHT FOOT, THIRD DIGIT
T8        RIGHT FOOT, FOURTH DIGIT
T9        RIGHT FOOT, FIFTH DIGIT
LT        LEFT SIDE
RT        RIGHT SIDE
LC        LEFT CIRCUMFLEX CORONARY ARTERY
LD        LEFT ANTERIOR DESC CIRCUMFLEX CORONARY
RC        RIGHT CORONARY ARTERY
58        STAGED OR RELATED PROCEDURE OR SERVICE
59        DISTINCT PROCEDURAL SERVICE
78        RETURN TO THE O.R FOR A RELATED PROCED
