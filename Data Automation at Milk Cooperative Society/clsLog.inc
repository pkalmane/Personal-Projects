'========================================================================================
'clsLOG.inc
'Creating a class that saves Log records
'========================================================================================

#INCLUDE ONCE "PBReadWriteINIFile.inc"
#INCLUDE ONCE "MACROS.INC"

'========================================================================================
CLASS clsDebugComport

INSTANCE iDebugComport AS LONG

CLASS METHOD Destroy
IF iDebugComport<>0 THEN
    CLOSE #iDebugComport
END IF
END METHOD
'-------------------------------------------------------
    INTERFACE DebugComportInterface
    INHERIT IUNKNOWN

'-------------------------------------------------------
METHOD Init(iPort AS LONG)
LOCAL s1 AS STRING
iDebugComport=iPort
IF iPort<>0 THEN
    s1="COM"+TRIM$(STR$(iPort))
    iDebugComPOrt=FREEFILE
    ON ERROR GOTO Debugcomportnotpresent
    COMM OPEN s1 AS #iDebugComPOrt
    IF ERRCLEAR THEN
        MSGBOX "Debug port "+s1+" not present. Disable Debug port in INI file" ,,$AppTitle
        iDebugComport=0
    ELSE

        COMM SET #iDebugComPOrt, BAUD     = 1200     ' 9600 baud
        COMM SET #iDebugComPOrt, BYTE     = 8        ' 8 bits
        COMM SET #iDebugComPOrt, PARITY   = %TRUE    ' Even parity
        COMM SET #iDebugComPOrt, PARITYTYPE   = 2    ' Even parity
        COMM SET #iDebugComPOrt, STOP     = 0        ' 1 stop bit
        COMM SET #iDebugComPOrt, TXBUFFER = 4096     ' 4k transmit buffer
        COMM SET #iDebugComPOrt, RXBUFFER = 4096     ' 4k receive buffer

    END IF
END IF
EXIT METHOD
Debugcomportnotpresent:
        MSGBOX "Debug port "+s1+" not present. Disable Debug port in INI file" ,,$AppTitle
        iDebugComport=0
        ON ERROR GOTO 0

END METHOD


'------------------------------------------------------
'function to send message to Debug Comport, if comport has been initialised
'SUB DebugComPortMessage(sMessage as string)
'input : Message String
'returns: nothing
'------------------------------------------------------
METHOD PrintItS(sMessage AS STRING)
IF iDebugComport<>0 THEN
   COMM SEND #iDebugCOmPort,sMessage
END IF
END METHOD

'------------------------------------------------------
'------------------------------------------------------
METHOD PrintItL(dw1 AS LONG)
IF iDebugComport<>0 THEN
   COMM SEND #iDebugCOmPort,trimstr(dw1)
END IF
END METHOD
'-------------------------------------------------------
  END INTERFACE
'-------------------------------------------------------
END CLASS

'========================================================================================
CLASS clsDate

CLASS METHOD Destroy
    'DCM("Destroy")
END METHOD
'-------------------------------------------------------
    INTERFACE DateInterface
    INHERIT IUNKNOWN

'-------------------------------------------------------
   METHOD sddmmyyyyS(sDate AS STRING) AS STRING
    LOCAL s1 AS STRING

    s1=sDate
    s1=MID$(s1,4,2)+"-"+LEFT$(s1,2)+"-"+RIGHT$(s1,4)
    METHOD=s1
   END METHOD

'-------------------------------------------------------
   METHOD sddmmyyyy() AS STRING
    LOCAL s1 AS STRING

    s1=DATE$
    s1=MID$(s1,4,2)+"-"+LEFT$(s1,2)+"-"+RIGHT$(s1,4)
    METHOD=s1
   END METHOD

'-------------------------------------------------------
   METHOD sddmmyyyyhhmmss() AS STRING
    LOCAL s1 AS STRING

    s1=DATE$
    s1=MID$(s1,4,2)+"-"+LEFT$(s1,2)+"-"+RIGHT$(s1,4)+","+TIME$
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD syyyymmdd() AS STRING
    LOCAL s1 AS STRING

    s1=DATE$
    s1=RIGHT$(s1,4)+"-"+LEFT$(s1,2)+"-"+MID$(s1,4,2)
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD syyyymmddhhmmss() AS STRING
    LOCAL s1 ,s2 AS STRING

    s1=DATE$
    s1=RIGHT$(s1,4)+"-"+LEFT$(s1,2)+"-"+MID$(s1,4,2)
    s2=TIME$
    REPLACE ANY ":" WITH "-" IN s2
    s1=s1+"-"+s2
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD ConvertSecondsToHHMMSS(lValueSeconds AS LONG) AS STRING
    LOCAL s1 ,s2 ,s3 AS STRING
    LOCAL l1,lHour,lMin, lSec AS LONG
    l1=INT(lValueSeconds/60)
    lSec=lValueSeconds MOD 60
    lHour=INT(l1/60)
    lMin=l1 MOD 60

    s1="00"+TRIMSTR(lHour)
    s1=RIGHT$(s1,2)
    s2="00"+TRIMSTR(lMin)
    s2=RIGHT$(s2,2)
    s3="00"+TRIMSTR(lSec)
    s3=RIGHT$(s3,2)
    s1=s1+":"+s2+":"+s3
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD ConvertSecondsToHHHHMM(lValueSeconds AS LONG) AS STRING
    LOCAL s1 ,s2 AS STRING
    LOCAL l1,lHour,lMin AS LONG
    l1=INT(lValueSeconds/60)
    lHour=INT(l1/60)
    lMin=l1 MOD 60
    IF (lValueSeconds-l1*60)>0 THEN INCR lMin

    s1="0000"+TRIMSTR(lHour)
    s1=RIGHT$(s1,4)
    s2="00"+TRIMSTR(lMin)
    s2=RIGHT$(s2,2)
    s1=s1+":"+s2
    METHOD=s1
   END METHOD
'-------------------------------------------------------
   METHOD ConvertHoursToHHHH(lValueHours AS LONG) AS STRING
    LOCAL s1 ,s2 AS STRING
    LOCAL l1,lHour,lMin AS LONG
    lHour=lvalueHours

    s1="0000"+TRIMSTR(lHour)
    s1=RIGHT$(s1,4)
    METHOD=s1
   END METHOD

'-------------------------------------------------------
   METHOD ConvertNumberYYYYMMDD(lNumber AS LONG) AS STRING
    LOCAL s1 ,s2 AS STRING
    s1=TRIM$(STR$(lNumber))
    s2=RIGHT$(s1,2)+"-"+MID$(s1,5,2)+"-"+LEFT$(s1,4)
    METHOD=s2
   END METHOD

'-------------------------------------------------------
  END INTERFACE
'-------------------------------------------------------
END CLASS


'========================================================================================
CLASS clsLogMessageList


'-------------------------------------------------------
    INTERFACE LogMessageListInterface
    INHERIT IUNKNOWN

'-------------------------------------------------------
    PROPERTY SET ServerMessage(sMessage AS STRING)

    END PROPERTY
'-------------------------------------------------------
    PROPERTY SET ActivityMessage(sMessage AS STRING)

    END PROPERTY
'-------------------------------------------------------
  END INTERFACE
'-------------------------------------------------------
END CLASS


'========================================================================================
CLASS clsServerLOG

    INSTANCE SystemDate AS DateInterface
    INSTANCE iLogFileNumber AS INTEGER

'-------------------------------------------------------

'-------------------------------------------------------
CLASS METHOD CREATE
'    me.WriteMessage("Start")
END METHOD
'-------------------------------------------------------
CLASS METHOD Destroy
'LOCAL s1, sLogFilename AS STRING
'LOCAL iFileNumber AS LONG
'
'    sLogFilename=EXE.PATH$+"Report\Server.LOG"
'    iFileNumber=FREEFILE
'    OPEN sLogFilename FOR APPEND AS #iFileNumber
'    s1=SystemDate.sddmmyyyyhhmmss
'    PRINT #iFileNumber, s1;",";"End"
'    CLOSE #iFileNumber

END METHOD

'-------------------------------------------------------
CLASS METHOD WriteMessage(sMessage AS STRING)
LOCAL s1, sLogFilename AS STRING
LOCAL iFileNumber AS LONG

    sLogFilename=EXE.PATH$+"Report\Server.LOG"
    iFileNumber=FREEFILE
    OPEN sLogFilename FOR APPEND AS #iFileNumber
    s1=SystemDate.sddmmyyyyhhmmss
    PRINT #iFileNumber, s1;",";sMessage
    CLOSE #iFileNumber
END METHOD
'-------------------------------------------------------


    INTERFACE ServerLogInterface
    INHERIT IUNKNOWN

'-------------------------------------------------------
   METHOD Init(sMessage AS STRING) AS INTEGER
   LOCAL sLogFilename AS STRING
   LOCAL iFileNumber AS INTEGER
   LOCAL s1,s2 AS STRING

    LET SystemDate=CLASS "clsDate"

    sLogFilename=EXE.PATH$+"Report\Server.LOG"
    iFileNumber=FREEFILE

    OPEN sLogFilename FOR APPEND AS #iFileNumber
    s1=SystemDate.sddmmyyyyhhmmss
    PRINT #iFileNumber, s1;",Server Started"
    PRINT #iFileNumber, s1;",";sMessage
    CLOSE #iFileNumber
    METHOD=1

   END METHOD

'-------------------------------------------------------
   METHOD WriteLog(sMessage AS STRING)
   LOCAL sLogFilename AS STRING
   LOCAL iFileNumber AS INTEGER
   LOCAL s1,s2 AS STRING

    me.WriteMessage(sMessage)

   END METHOD

'-------------------------------------------------------

  END INTERFACE
'-------------------------------------------------------

END CLASS

'========================================================================================
CLASS clsActivityLOG

    INSTANCE SystemDate AS DateInterface
    INSTANCE iDateInitialisedFlag AS LONG

'-------------------------------------------------------
CLASS METHOD CREATE
iDateInitialisedFlag=0
END METHOD
'-------------------------------------------------------

    INTERFACE ActivityLogInterface
    INHERIT IUNKNOWN

'-------------------------------------------------------
   METHOD WriteLog(sMessage AS STRING)
   LOCAL sLogFilename AS STRING
   LOCAL iFileNumber AS INTEGER
   LOCAL s1,s2 AS STRING

    IF iDateInitialisedFlag=0 THEN
        iDateInitialisedFlag=1
        LET SystemDate=CLASS "clsDate"
    END IF

    s1=SystemDate.syyyymmdd
    sLogFilename=EXE.PATH$+"Report\"+s1+".LOG"
    iFileNumber=FREEFILE
    OPEN sLogFilename FOR APPEND AS #iFileNumber
    s1=SystemDate.sddmmyyyyhhmmss
    PRINT #iFileNumber, s1;",";sMessage
    CLOSE #iFileNumber

   END METHOD

'-------------------------------------------------------

  END INTERFACE
'-------------------------------------------------------

END CLASS


'========================================================================================
CLASS clsLOG

    INSTANCE SystemDate AS DateInterface
    INSTANCE sLogFilename AS STRING
    INSTANCE iLogfileInitialised AS INTEGER
'-------------------------------------------------------

'-------------------------------------------------------
    INTERFACE LogInterface

    INHERIT IUNKNOWN

'-------------------------------------------------------
   METHOD InitLog(sName AS STRING)



   LOCAL iFileNumber AS INTEGER
   LOCAL s1,s2 AS STRING

    LET SystemDate=CLASS "clsDate"

    s1=SystemDate.syyyymmdd
    sLogFilename=EXE.PATH$+sName+".LOG"
    iFileNumber=FREEFILE
    OPEN sLogFilename FOR APPEND AS #iFileNumber
    s1=SystemDate.sddmmyyyyhhmmss
    PRINT #iFileNumber, s1;",Logfile Opened"
    CLOSE #iFileNumber
    iLogfileInitialised=1

   END METHOD

'-------------------------------------------------------

'-------------------------------------------------------
   METHOD WriteLog(sMessage AS STRING)
   LOCAL iFileNumber AS INTEGER
   LOCAL s1,s2 AS STRING

    LET SystemDate=CLASS "clsDate"

    s1=SystemDate.syyyymmdd
    IF iLogfileInitialised<>1 THEN
        sLogFilename=EXE.PATH$+s1+".LOG"
    END IF
    iFileNumber=FREEFILE
    OPEN sLogFilename FOR APPEND AS #iFileNumber
    s1=SystemDate.sddmmyyyyhhmmss
    PRINT #iFileNumber, s1;",";sMessage
    CLOSE #iFileNumber

   END METHOD

'-------------------------------------------------------

  END INTERFACE
'-------------------------------------------------------

END CLASS
