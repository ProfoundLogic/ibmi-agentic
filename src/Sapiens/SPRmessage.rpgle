     DdsRtvMsgInfo     DS                  qualified
     D type                           7    inz('*LAST')                         *LAST/*PRV/*FIRST/*N
     D pos                           10i 0 inz(*hival)
     D pgm                           10    inz('%')
     D lib                           10    inz('%')
     ddsRtnMsgInfo     ds                  qualified inz
     D pos                           10i 0
     D msgId                          7
     D msgText                     1024
     D*=====================================================================
     D* String handling procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* sndPgmMsg - sends a program message to the current program
     P*             message queue.
     P*  Parameter 1 - 10 character message ID
     P*  Parameter 2 - 10 character message type
     P*  Parameter 3 - 10 character message file
     P*  Parameter 4 - 10 character message file library
     P*  Parameter 5 - varible character message data
     P*  Returns - nothing
     P*=====================================================================
     DsndPgmMsg        PR                  opdesc
     D msgInId                       10    const
     D msgInType                     10    const
     D msgFile                       10    const
     D msgLib                        10    const
     D msgInData                  32766    const options(*varsize:*nopass)
     D
     P*=====================================================================
     P* rmvMsg - remove program messages
     P*  Parameter 1 - 10 character message ID
     P*  Returns - nothing
     P*=====================================================================
     DrmvMsg           PR
     D rmvID                         10    const options(*nopass)
     D
     P*=====================================================================
     P* RtvMsg - retrieve message
     P*  Parameter 1 - job info (job#/user/job) (* for current)
     P*  Parameter 2 - run info
     P*                 type - *FIRST/*LAST/*PRV/*NEXT
     P*                 pos  - position to start from
     P*                 pgm  - program sending the message
     P*                 lib  - library sending the message
     P*  Parameter 3 - message type (optional)
     P*  Returns - message info
     P*                 pos  - position of message
     P*                 id   - message id
     P*                 text - message text
     P*=====================================================================
     DRtvMsg           pr          1050
     D inJob                         28    const options(*nopass)
     D inInfo                              const options(*nopass)
     D                                     likeds(dsRtvMsgInfo)
     D*** Begin Add *** 04/27/22 *******************************************
     D inMessageType                 13    const varying options(*nopass)
     D*** End   Add *** 04/27/22 *******************************************
     P*=====================================================================
     P* RtvMsgD - retrieve message description
     P*  Parameter 1 - input message file name
     P*  Parameter 2 - input message ID
     P*  Returns - message decription
     P*=====================================================================
     DRtvMsgD          pr           500
     D inMsgF                        10    const options(*nopass)
     D inMsgID                        7    const options(*nopass)
