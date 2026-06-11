     h*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     h**********************************************************************************************
     h/TITLE DA702R - Close Remark
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:  This program will close a remark based upon the key  ords, and outputs an   *
     h*                and remark code.                                      and new payroll       *
     h*                                                                                            *
     h*  Called from:  F0260R - Medical Case Information                                           *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 07/30/2006 by c:62 for Project 139MEL                                          *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h**********************************************************************************************
     d*=============================================================================================
     d* Stand alone fields
     d*=============================================================================================
     d eKey            s             40
     d eRemarkCode     s              5
     d eCloseDate      s              7  0
     d eRtcd           s              1
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    eKey
     c                   parm                    eRemarkCode
     c                   parm                    eCloseDate
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c/EXEC SQL
     c+ UPDATE SDCMP
     c+ SET CMDCDT = :eCloseDate
     c+ WHERE CMKEY = :eKey AND CMRMCD = :eRemarkCode
     c/END-EXEC

     c                   return
