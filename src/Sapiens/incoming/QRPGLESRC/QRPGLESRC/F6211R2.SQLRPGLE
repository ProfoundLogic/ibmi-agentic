     h*  (C) Copyright StoneRiver, 2009
     h**********************************************************************************************
     h/TITLE F6211R2 - Delete Bad FEF1P Records
     h option(*nodebugio:*srcstmt)
     h**********************************************************************************************
     h*  DESCRIPTION:  This program reads the flat file WXPMP, edits the records, and outputs an   *
     h*                error report or a successful upload verication report and new payroll       *
     h*                classification report. This program can process one header and one trailer  *
     h*                record in each flat file only.                                              *
     h*                                                                                            *
     h*  Called from:  F6211C2                                                                     *
     h*  Calls      :  None                                                                        *
     h*                                                                                            *
     h**********************************************************************************************
     h*  Created on 03/27/2009 by c:62 for Project 67020                                           *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*
     h**********************************************************************************************
     d*=============================================================================================
     d* Stand alone fields
     d*=============================================================================================
     d rrn05           s              9  0
     d rrn06           s              9  0
     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c*
     c/EXEC SQL
     c+ DECLARE PROJECT CURSOR FOR
     c+  select rrn(a) as bad05, rrn(a) + 1 as bad06
     c+  from fef1p a join fef1p b  on rrn(a)=rrn(b)-2
     c+  where substr(a.f1data,1,2)='05' and
     c+        substr(b.f1data,1,2)<>'08'
     c/END-EXEC
     c*
     c/EXEC SQL
     c+ OPEN PROJECT
     c/END-EXEC
     c*
     c                   if        sqlstt = '00000'
     c*
     c/EXEC SQL
     c+ FETCH NEXT
     c+ FROM PROJECT
     c+ INTO  :RRN05, :RRN06
     c/END-EXEC
     c*
     c                   endif
     c*
     c                   if        sqlstt = '00000'
     c/EXEC SQL
     c+ delete from fef1p
     c+ where rrn(fef1p)=:rrn05 or rrn(fef1p)=:rrn06
     c/END-EXEC
     c                   endif

     c                   eval      *inlr = *on
