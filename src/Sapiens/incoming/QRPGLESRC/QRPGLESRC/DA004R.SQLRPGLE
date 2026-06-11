     h*  (C) Copyright StoneRiver, Inc., 2010
     h**********************************************************************************************
     h/TITLE DA003R - Retrieve Clob
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  Description:  This program will retrieve 32K sections of clob field                       *
     h*                                                                                            *
     h*  Parameters:                                                                               *
     h*      eKey      - Darts Key            : Required   <== IN                                  *
     h*      eSeq#     - Darts Sequence #     : Required   <== IN                                  *
     h*      eAltSeq#  - Alternate Sequence # : Optional   <== IN/OUT                              *
     h*            =If 0 then get most current alt seq#                                            *
     h*            =If not 0 then use that seq#                                                    *
     h*            =If 0 then pass back seq#                                                       *
     h*      eLength   - Length of Clob       : Required   <== IN/OUT                              *
     h*                                                                                            *
     h*  Called from:  ?                                                                           *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 01/18/2010 by c:62 for Project 135WNX                                          *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h**********************************************************************************************
     D psds          esds                  extname(szq1p)
    
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d eKey            s             40
     d eSeq#           s              5  0
     d eAltSeq#        s              5  0
     d ePosition       s             15  0
     d eLength         s             15  0
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    eKey
     c                   parm                    eSeq#
     c                   parm                    eAltSeq#
     c                   parm                    eLength
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c                   if        eAltSeq#=0
     c/EXEC SQL
     c+ SELECT MAX(CLASEQ)
     c+ INTO :eAltSeq#
     c+ FROM SMCLP
     c+ WHERE CLKEY = :eKey AND CLSEQ = :eSeq#
     c/END-EXEC
     c                   endif
     c*
     c/EXEC SQL
     c+ SELECT LENGTH(CLCLOB)
     c+ INTO :eLength
     c+ FROM SMCLP
     c+ WHERE CLKEY = :eKey AND CLSEQ = :eSeq# and CLASEQ=:eAltSeq#
     c/END-EXEC

     c                   return
