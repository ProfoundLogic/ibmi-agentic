     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
      *================================================================
      *
      *  This module
      *
      * 1)receives
      *   - the object-path-name of an IFS object
      * 2)returns
      *   - existence indicator: "1"-found, "0"-not found
      *   - object size in byte (if found, otherwise 0)
      *   - object creation time stamp (if found, otherwise 0001-01-01)
      *   - object type
      *   - code page
      *
      *====================================================================
      *  Revisions:
      *
      *  01/17/13  C:33  Project: SY266 #2040
      *                 -If the process fails to find the object,
      *
      *  09/12/11  C:33  Seq#: OP099/77724
      *                 -Remove hardcoded libraries
      *                 -Use stat64 API as opposed to stat
      *====================================================================
      /copy qrpglesrc,prototypeb
      *===================================================================
      * Prototype and interface for this module
     D Chk             pr
     D  IfsObjName                 1024a
     D  IfsObjFnd                      n
     D  IfsObjSize                   10i 0
     D  IfsCrtStam                     z
     D  IfsObjType                   10a
     D  IfsCodPage                   10a

     D Chk             pi
     D  IfsObjName                 1024a
     D  IfsObjFnd                      n
     D  IfsObjSize                   10i 0
     D  IfsCrtStam                     z
     D  IfsObjType                   10a
     D  IfsCodPage                   10a

      * Variables used to compute
     D***  Begin Add  ***  01/17/13  **************************************
     D statResults64   s             10i 0
     D***  End   Add  ***  01/17/13  **************************************
     D Char_Epoch      s             26a   inz('1970-01-01-00.00.00.000000')
     D Epoch           s               z
     D SecSince        s               t
     D Char_0Stamp     s             26a   inz('0001-01-01-00.00.00.000000')
      *
      *
      * Buffer for IFS stat() function
      * See http://publib.boulder.ibm.com/pubs/html/as400/v5r1/ic2924/index.htm?info/apis/unix2.htm
     D StatusBuffer    ds                  align
     D  StsPermissions...
     D                               10u 0
     D  StsFileID                    10u 0
     D  StsLinkCount                  5u 0
     D  StsUserIDNbr                 10u 0
     D  StsGroupIdNbr                10u 0
      *The number of data bytes in the file
     D  StsBytes                     10i 0
      *The most recent time the file was accessed
     D  TimLstAcc                    10i 0
      *The most recent time the contents of the file were changed
     D  TimLstChg                    10i 0
      *The most recent time the status of the file was changed
     D  TimSLstChg                   10i 0
     D  StsFileSysID                 10u 0
     D  StsBlockSize                 10u 0
     D  StsAllocBytes                10u 0
     D  StsObjectType                11
     D  StsCodePage                   5u 0
     D                               62
     D  StsGenerationID...
     D                               10u 0
      *================================================================
      * If file not found
     C***  Begin Chg  ***  01/17/13  **************************************
     C**********         IF        stat64(%addr(IfsObjName):
     C**********                   %addr(StatusBuffer)) = -1
      /Free
       statResults64 = stat64(%trim(ifsObjName):%addr(statusBuffer));

       If statResults64 = -1 and
        %subst(ifsObjName:1:1) <> '/';
          ifsObjName = '/' + %trim(ifsObjName);
          statResults64 = stat64(%trim(ifsObjName):%addr(statusBuffer));
       Endif;

       If statResults64 = -1;
      /End-free
     C***  End   Chg  ***  01/17/13  **************************************

     C                   eval      IfsObjFnd  = '0'
     C                   eval      IfsObjSize = 0
     C                   move      Char_0Stamp   IfsCrtStam
     C                   eval      IfsObjType = ' '
     C                   eval      IfsCodPage = ' '
     C                   ELSE
     C                   eval      IfsObjFnd  = '1'
     C                   eval      IfsObjSize = StsBytes
     C                   eval      IfsObjType = %subst(StsObjectType:1:10)
     C                   eval      IfsCodPage = %editc(StsCodePage:'Z')
      * Compute the timestamp of the file creation date.
      * -Use field "TimSLstChg"
      * -All date fields are in seconds since "Epoch",
      *  1st January 1970 at hours 00:00
     C                   move      Char_Epoch    Epoch
     C     Epoch         adddur    TimSLstChg:*s IfsCrtStam
     C                   ENDIF
      *
     C                   return
