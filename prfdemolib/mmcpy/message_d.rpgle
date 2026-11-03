      // @MBRTEXT: Message subfile definition copy member
      ********************************************************************* ****
      * Application: General
      *     Purpose: Definitions for message subfile handling
      *
      * Log:
      * 08/01/01  Billy Andrews  Created
      *
      ********************************************************************* ****

      *--------------------------------------------------------------------
      * Global variables
      *--------------------------------------------------------------------
      * For message handling procedures
     d MsgF            s             10    import
     d MsgPre          s              3    import
     d MsgPgm          s             10    import
     d MsgModule       s             10    import
     d MsgProc         s             10    import
      * For display file
     d PgmQ            s             10    inz('*')

      *--------------------------------------------------------------------
      * Procedure prototypes
      *--------------------------------------------------------------------
      * Place message on program queue
     d sndMsg          pr
     d  msg#                          4    value
      * Clear queue
     d clrMsg          pr
