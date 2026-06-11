     h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2007
     h**********************************************************************************************
     h/TITLE DA703R - Transfer/Delete SMCLP records
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:  This program will transfer or delete SMCLP records.  ords, and outputs an   *
     h*                and remark code.                                      and new payroll       *
     h*                                                                                            *
     h*  Called from:  SC900R series - DARTS transfer                                              *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 04/24/2007 by c:62 for Project DR214                                           *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h**********************************************************************************************
     d*=============================================================================================
     d* Stand alone fields
     d*=============================================================================================
     D psds          esds                  extname(szq1p)
    
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d eFromKey        s             40
     d eToKey          s             40
     d eFromSeq#       s              5  0
     d eToSeq#         s              5  0
     d eCall           s              1

     d date            s              7  0

     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    eFromKey
     c                   parm                    eFromSeq#
     c                   parm                    eToKey
     c                   parm                    eToSeq#
     c                   parm                    eCall
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c                   time                    dsdttm
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         date

     c                   select
     c                   when      eCall = 'D'
     c/EXEC SQL
     c+ DELETE FROM SMCLP
     c+ WHERE CLKEY = :eFromKey AND CLSEQ = :eFromSeq#
     c/END-EXEC

     c                   when      eCall = 'C'
     c/EXEC SQL
     c+ INSERT INTO SMCLP (CLKEY, CLSEQ, CLASEQ, CLCLOB,
     c+ CLCDAT, CLCODE, CLTRDT, CLTRTM, CLSECL, CLSBYL,
     c+ CLSTAT, CLUSER, CLPGM, CLTIME, CLDT, CLIUSR,
     c+ CLRMCD, CLIDT)
     c+ (SELECT :eToKey, :eToSeq#, b.CLASEQ, b.CLCLOB,
     c+  b.CLCDAT, b.CLCODE, b.CLTRDT, b.CLTRTM, b.CLSECL,
     c+  b.CLSBYL, b.CLSTAT, b.CLUSER, :q1pgm, :dstime,
     c+  :date, :q1user, b.CLRMCD, :date
     c+ FROM SMCLP b
     c+ WHERE b.CLKEY = :eFromKey AND CLSEQ = :eFromSeq#)
     c/END-EXEC
     c                   other
     c/EXEC SQL
     c+ UPDATE SMCLP
     c+ SET CLKEY = :eToKey, CLSEQ = :eToSeq#,
     c+     CLPGM = :q1pgm, CLTIME = :dstime,
     c+     CLDT  = :date
     c+ WHERE CLKEY = :eFromKey AND CLSEQ = :eFromSeq#
     c/END-EXEC
     c                   endsl

     c                   return
