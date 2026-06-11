     h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2007
     h**********************************************************************************************
     h/TITLE TRFSISDRY - Transfer SIS Diaries to New Adjuster
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:  This program will transfer SIS Diaries to New Adjuster.                     *
     h*                                                                                            *
     h*  Called from:  Command                                                                     *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 03/04/2014 by c:62 for Project 114ILG                                          *
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

     d eFromUser       s             10
     d eToUser         s             10

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
     c                   parm                    eFromUser
     c                   parm                    eToUser
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
     c/EXEC SQL
     c+ UPDATE SDCMP
     c+ SET CMUSER = :eToUser, CMPGM = :q1pgm, CMTIME = :dstime,
     c+     CMDT  = :date
     c+ WHERE CMUSER = :eFromUser AND CMPNAM = 'SISCLM' AND
     c+       CMSTAT <> 'I' AND CMDCDT=0 AND CMDIFL='Y'
     c/END-EXEC

     c                   return
