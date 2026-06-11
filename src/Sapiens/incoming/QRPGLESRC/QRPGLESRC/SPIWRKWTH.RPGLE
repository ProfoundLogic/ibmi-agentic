     H copyright('Progressive Data Solutions, Inc 1999')
     H nomain
     h/define   PROFOUNDUI
     H**********************************************************************
     H* Revisions:
     H*
     h*  07/29/16 C:079 Support: PHW42622/99
     H*                 Remove OVRFILE
     H*
     h*  03/03/16 C:307 Support: PHW40126
     H*                 Added profound UI handler on the screen.
     H*
     H**********************************************************************
     H*** Beg Chg ******* 03/03/16 ******************************
     F***srrdckf   cf   e             workstn
     Fsrrdckf   cf   e             workstn
     F                                     handler('PROFOUNDUI(HANDLER)')
     H*** End Chg ******* 03/03/16 ******************************
     D*
     D/copy sprwrkwth
     D/copy sprruncmd
     P*=====================================================================
     P* readCheck - displays a panel asking user how to continue when few
     P*             records have met the subset criteria
     P*  Parameter 1 - 7 numeric (number of reads)
     P*  Parameter 2 - 3 numeric (number of selects)
     P*  Returns - character field ('1' to '5' that user entered)
     P*=====================================================================
     PreadCheck        B                   Export
     DreadCheck        PI             1
     D numOfReads                     7  0 const
     D numOfSels                      3  0 const
     D
    C*
     C                   eval      @reads = numOfReads
     C                   eval      @sel = numOfSels
     C                   dou       *in70 = *off
     C                   eval      *in70 = *off
     C                   exfmt     scr01
     C                   movea     '00000000'    *in(71)
     C                   if        *inkl = *off
     C                   exsr      errchk
     C                   else
     C                   move      '1'           answer
     C                   endif
     C                   enddo
     C                   return    answer
    C*
     C     errchk        begsr
     C                   if        answer < '1'
     C                             or answer > '5'
     C                   eval      *in70 = *on
     C                   eval      *in71 = *on
     C                   endif
     C                   endsr
     PreadCheck        E
     P
     P*** Beg Del *** 07/29/16 *******************************************
     P*=====================================================================
     P*  ovrFile - override file to use SHARE(*YES)
     P*  Parameter 1 - 10 character (file name)
     P*  Returns - nothing
     P*=====================================================================
     P*******ovrFile          B                   export
     D*******ovrFile          PI
     D******* fileName                      10    Const
     D*******
     D******* override        c                   'OVRDBF'
     D******* share           c                   'SHARE(*YES)'
     D******* cmd             s             33
     C*
     C*******     override      cat(p)    fileName:1    cmd
     C*******                   cat       share:1       cmd
     C*******                   callp     runCmd(cmd : 33)
     C*
     P*******ovrFile          E
     P*** End Del *** 07/29/16 *******************************************
     P
     P*=====================================================================
     P*  opnQryFile - execute OPNQRYF command
     P*  Parameter 1 - 10 character (file name)
     P*  Returns - nothing
     P*=====================================================================
     PopnQryFile       B                   export
     DopnQryFile       PI
     D fileName                      10    Const
     D qrySelect                    512    Const
     D
     D openQuery       c                   'OPNQRYF FILE(('
     D qrySelText      c                   ')) QRYSLT('
     D keyField        c                   ') KEYFLD(*FILE) OPTIMIZE(*FIRSTIO)'
     D cmd             s           1000
     C*
     C     openQuery     cat(p)    fileName:0    cmd
     C                   cat       qrySelText:0  cmd
     C                   cat       qrySelect:0   cmd
     C                   cat       keyField:0    cmd
     C                   callp     runCmd(cmd : 1000)
     C*
     PopnQryFile       E
     P
     P*=====================================================================
     P*  closeFile - close file ID opened by OPNQRYF
     P*  Parameter 1 - 10 character (file name)
     P*  Returns - nothing
     P*=====================================================================
     PcloseFile        B                   export
     DcloseFile        PI
     D fileName                      10    Const
     D
     D close           c                   'CLOF'
     D cmd             s             20
     C*
     C     close         cat(p)    fileName:1    cmd
     C                   callp     runCmd(cmd : 20)
     C*
     PcloseFile        E
     P
