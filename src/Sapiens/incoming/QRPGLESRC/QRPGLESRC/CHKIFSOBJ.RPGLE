      *=====================================================================
      * Check an IFS objects
      *
      * CRTPGM PGM(IFSTOOL/CHKIFSOBJ) MODULE(IFSTOOL/CHKIFSOBJ
      *        IFSTOOL/CHK IFSTOOL/SNDPGMMSG) ACTGRP(*CALLER)
      *
      *=====================================================================
      /copy *libl/qrpglesrc,hspecs
      /copy *libl/qrpglesrc,hspecsbnd
      /copy *libl/qrpglesrc,prototypeb
      /copy *libl/qrpglesrc,variables3
      /copy *libl/qrpglesrc,usec
     D IfsObj          s            256a
     D ObjSize         s             15a
     D ObjDate         s             10a
     D ObjTime         s              8a
     D Msgid           s              7a
     D MsgDta          s            500a
     D MsgType         s              7a
     D  IfsObjNam      s           1024a
     D  IfsObjFnd      s               n
     D  IfsObjSize     s             10i 0
     D  IfsCrtStamp    s               z
     D  IfsObjType     s             10a
     D  IfsCodPage     s             10a
     D IfsCrtStamC     s             26a
      *=====================================================================
      * Main line
      *=====================================================================
     C     *entry        plist
     C                   parm                    IfsObj
     C                   parm                    IfsObjFnd
      * Check object
     C                   exsr      ChkObj
      * Send program message
     C                   IF        IfsObjFnd = *off
     C                   eval      msgid   = 'CPF9898'
     C                   eval      MsgType = '*INFO'
     C                   eval      MsgDta = IfsObj + ' Not Found.'
     C                   ELSE
     C                   move      IfsCrtStamp   IfsCrtStamC
     C                   eval      ObjDate = %subst(IfsCrtStamC:01:10)
     C                   eval      ObjTime = %subst(IfsCrtStamC:12:08)
     C                   eval      ObjSize = %editc(IfsObjSize:'Z')
     C                   eval      msgid   = 'CPF9898'
     C                   eval      MsgType = '*INFO'
     C                   eval      MsgDta = 'Created/changed ' + Objdate
     C                                      + ' at ' + ObjTime + ', type '
     C                                      + Ifsobjtype + ', size ' +  Objsize
     C                                  + ', code page ' +  IFScodpage + ' for '
     C                                      + ifsobj
     C                   ENDIF
     C                   exsr      SndPgmMsg
      * Return
     C                   return
      *=====================================================================
      * Check IFS object
      *=====================================================================
     C     ChkObj        begsr
     C                   eval      IfsObjNam = IfsObj
     C                   callb     'CHK'
     C                   parm                    IfsObjNam
     C                   parm                    IfsObjFnd
     C                   parm                    IfsObjSize
     C                   parm                    IfsCrtStamp
     C                   parm                    IfsObjType
     C                   parm                    IfsCodPage
     C                   endsr
      *=====================================================================
      * Send program message
      *=====================================================================
     C     SndPgmMsg     begsr
     C                   callb     'SNDPGMMSG'
     C                   parm                    MsgId
     C                   parm                    MsgDta
     C                   parm                    MsgType
     C                   endsr
