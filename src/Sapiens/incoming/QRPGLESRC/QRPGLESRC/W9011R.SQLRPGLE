     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     h**********************************************************************************************
     h/TITLE W9011R - Test Environment Refresh
     h**********************************************************************************************
     h*  Description:  This program will update the test environment with   ords, and outputs an   *
     h*                 live information for each group selected.            and new payroll       *
     h**********************************************************************************************
     h*  Created: 10/01/09   C:33   Project: 114WUO                                                *
     h**********************************************************************************************
     h* Revisions:                                                                                 *
     h*                                                                                            *
     h* 03/10/10 C:191 Project: 114WUO                                                             *
     h*                SNDPGMMSG not allowed on QCMDEXC. Change to use API                         *
     h*                                                                                            *
     h**********************************************************************************************
     Fwxgsl     if   e           k disk
     Fwattp     if   e           k disk
    
     D psds          esds                  extname(szq1p)

     d @@arcLib        s             10
     d cltID           s              3
     d suffix          s              3
     d arcLib          s             10    inz('PDSTFR')
     d fromLib         s             10    inz('PDSFIL')
     d toLib           s             10    inz('PDSTST')
     d cmd             s            255
     d interactive     s              1n

     dCrtArcCmd        pr                  like(cmd)
     d file                                const like(TTFILE)
     d tolib                               const like(TTTLIB)

     dCrtCpyStmt       pr                  like(cmd)
     d file                                const like(TTFILE)
     d key1                                const like(TTKEY1)
     d key2                                const like(TTKEY2)
     d key3                                const like(TTKEY3)
     d fromLib                             const like(TTFLIB)
     d toLib                               const like(TTTLIB)
     d co#                                 const like(GSCO#)
     d fnd                                 const like(GSFND)

     dDeleteRcds       pr
     d file                                const like(TTFILE)
     d key1                                const like(TTKEY1)
     d key2                                const like(TTKEY2)
     d key3                                const like(TTKEY3)
     d toLib                               const like(TTTLIB)
     d co#                                 const like(GSCO#)
     d fnd                                 const like(GSFND)

     DChkInteract      PR                  extpgm('SRCHKPGM')
     D pgmName                       10    const
     D exists                         1

     DRtvCltID         PR                  extpgm('RTVCLTID')
     D cltID                          3

     DRtvSuffix        PR                  extpgm('RTVSUFFIX')
     D suffix                         3

     D*** BEGIN ADD ****** 03/10/10 ************************************
     D SendErrMsg      pr                  extpgm('QMHSNDPM')
     D   MsgID                        7    const
     D   MsgFile                     20    const
     D   MsgDta                      80    const
     D   MsgDtaLen                   10i 0 const
     D   MsgType                     10    const
     D   MsgQ                        10    const
     D   MsgQNbr                     10i 0 const
     D   MsgKey                       4
     D   ErrorDS                     16

     D ErrorDS         ds            16
     D   BytesProv                   10i 0 inz(16)
     D   BytesAvail                  10i 0
     D   ExceptionID                  7

     D MsgDta          s             80
     D MsgKey          s              4
     D*** END   ADD ****** 03/10/10 ************************************

     d/copy sPrRunCmd

     c     *entry        plist
     c                   parm                    @@arcLib

     c                   callp     ChkInteract('W9010C':interactive)
     c                   callp     RtvCltID(cltID)
     c                   callp     RtvSuffix(suffix)
     c                   eval      arcLib  = %trim(arcLib)  + suffix
     c                   eval      fromLib = %trim(fromLib) + suffix
     c                   eval      toLib   = %trim(toLib)   + suffix

     c                   if        %parms >= 1
     c                   eval      arcLib = @@arcLib
     c                   endif

     c*=============================================================================================
     c* Main Processing
     c*=============================================================================================
     c     *loval        setll     wxgsl
     c                   dou       %eof(wxgsl)
     c                   read      wxgsl
     c                   if        not %eof(wxgsl) and GSIEFL = 'I'
     c                   exsr      ProcessGrp
     c                   endif
     c                   enddo
     c*
     c                   eval      *inlr = *on
     c                   return

     c*=============================================================================================
     c* Process each group
     c*=============================================================================================
     c     ProcessGrp    begsr
     c*
     c     *loval        setll     wattp
     c                   dou       %eof(wattp)
     c                   read      wattp
     c                   if        not %eof(wattp) and TTSTAT = 'A'
     c                   if        TTCLT# = *blanks or TTCLT# = cltID
     c                   exsr      Process
     c                   endif
     c                   endif
     c                   enddo
     c*
     c                   endsr

     c*=============================================================================================
     c* Processing
     c*=============================================================================================
     c     Process       begsr
     c*
     c                   if        TTFLIB = *blanks
     c                   eval      TTFLIB = fromLib
     c                   endif
     c*
     c                   if        TTTLIB = *blanks
     c                   eval      TTTLIB = toLib
     c                   endif
     c*
     c* Archive current test file
     c                   eval      cmd = CrtArcCmd(TTFILE:
     c                                             TTTLIB)
     c                   callp     RunCmd(cmd:%len(cmd))
     c*
     c* Remove group from current test file
     c                   callp     DeleteRcds(TTFILE:
     c                                        TTKEY1:
     c                                        TTKEY2:
     c                                        TTKEY3:
     c                                        TTTLIB:
     c                                        GSCO# :
     c                                        GSFND )
     c*
     c* Copy group from live to test file
     c                   eval      cmd = CrtCpyStmt(TTFILE:
     c                                              TTKEY1:
     c                                              TTKEY2:
     c                                              TTKEY3:
     c                                              TTFLIB:
     c                                              TTTLIB:
     c                                              GSCO# :
     c                                              GSFND )
     c                   callp     RunCmd(cmd:%len(cmd))

     c                   endsr





     p*=============================================================================================
     p* Create Archive command
     p*=============================================================================================
     pCrtArcCmd        b
     dCrtArcCmd        pi                  like(cmd)
     d file                                const like(TTFILE)
     d toLib                               const like(TTTLIB)

     d outCmd          s                   like(cmd)

     c                   eval      outCmd = 'ARCOBJ OBJ(' +
     c                                      %trim(toLib) + '/' +
     c                                      %trim(file) + ') ' +
     c                                      'OBJTYPE(*FILE) TOLIB(' +
     c                                      %trim(arcLib) + ')'

     c                   return    outCmd

     pCrtArcCmd        e





     p*=============================================================================================
     p* Create Copy statement
     p*=============================================================================================
     pCrtCpyStmt       b
     dCrtCpyStmt       pi                  like(cmd)
     d file                                const like(TTFILE)
     d key1                                const like(TTKEY1)
     d key2                                const like(TTKEY2)
     d key3                                const like(TTKEY3)
     d fromLib                             const like(TTFLIB)
     d toLib                               const like(TTTLIB)
     d co#                                 const like(GSCO#)
     d fnd                                 const like(GSFND)

     d outCmd          s                   like(cmd)

     c                   eval      outCmd = 'CPYF FROMFILE('
     c                                    + %trim(fromLib) + '/'
     c                                    + %trim(file)
     c                                    + ') TOFILE('
     c                                    + %trim(toLib) + '/'
     c                                    + %trim(file)
     c                                    + ') MBROPT(*ADD) '
     c                                    + 'INCREL((*IF '
     c                                    + %trim(key1) + ' *EQ '
     c                                    + %trim(%editc(co#:'Z'))
     c                                    + ') (*AND '
     c                                    + %trim(key2) + ' *EQ '
     c                                    + %trim(%editc(fnd:'Z'))
     c                                    + ')) FMTOPT(*DROP *MAP)'

     c                   return    outCmd

     pCrtCpyStmt       e





     p*=============================================================================================
     p* Delete records from files
     p*=============================================================================================
     pDeleteRcds       b
     dDeleteRcds       pi
     d file                                const like(TTFILE)
     d key1                                const like(TTKEY1)
     d key2                                const like(TTKEY2)
     d key3                                const like(TTKEY3)
     d toLib                               const like(TTTLIB)
     d co#                                 const like(GSCO#)
     d fnd                                 const like(GSFND)

     d msg             s            256a
     d pgmErr          s               n   inz(*off)
     d sqlStmt         s            256a
     d sqlSt           s                   like(sqlStmt)
     d sqlSttOk        c                   const( '00000' )
     d sqlSttNoRow     c                   const( '02000' )

     c*** BEGIN CHG ****** 03/10/10 ************************************
     c************       eval      sqlStmt = 'delete * from '
     c                   eval      sqlStmt = 'delete from '
     c*** END   CHG ****** 03/10/10 ************************************
     c                                     + %trim(toLib) + '/'
     c                                     + %trim(file)
     c                                     + ' where '
     c                                     + %trim(key1)
     c                                     + ' = ' + %trim(%editc(co#:'Z'))
     c                                     + ' and '
     c                                     + %trim(key2)
     c                                     + ' = ' + %trim(%editc(fnd:'Z'))
     c*************                        + ';'
     c                   eval      sqlSt = sqlStmt

     c/exec SQL
     c+                  execute immediate :sqlSt
     c/end-exec

     c                   if        sqlStt <> SqlSttOK and
     c                             sqlStt <> sqlSttNoRow
     c                   eval      pgmErr = *on
     c                   eval      msg = %trim(file)
     c                                 + ' - Delete failed:'
     c                                 + '  SQLSTT = ' + sqlStt
     c                   select
     c                   when      interactive
     c                   eval      cmd = 'SNDMSG'
     c                   eval      cmd = %trim(cmd) + ' MSG('''
     c                                 + %trim(msg) + ''')'
     c                   callp     RunCmd(cmd:%len(cmd))
     c                   other
     c*** BEGIN CHG ****** 03/10/10 ************************************
     c*************      eval      cmd = 'SNDPGMMSG'
     c                   eval      MsgDta = %trim(msg)
     C                   callp     SendErrMsg ('CPF9898':
     C                               'QCPFMSG   QSYS':
     C                               MsgDta:
     C                               %len(MsgDta):
     C                               '*INFO':
     C                               '*':
     C                               2:
     C                               MsgKey:
     C                               ErrorDS)
     c*** END   CHG ****** 03/10/10 ************************************
     c                   endsl
     c                   endif

     c                   return

     pDeleteRcds       e
