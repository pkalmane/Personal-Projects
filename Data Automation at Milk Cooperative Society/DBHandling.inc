'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"

'------------------------------------------------------------------------------
' Equates
'------------------------------------------------------------------------------
'$AppTitle = "Address Book - PBWin DDT Example"

' Edit Control IDs

'------------------------------------------------------------------------------
FUNCTION ReadMasterFile() AS LONG
LOCAL  i,j,k   AS LONG
LOCAL    lFile   AS LONG
LOCAL s1,s2,s3, sFile AS STRING

lFile=FREEFILE

IF ISFILE($MasterFIle)=0 THEN ' file not present
    lFarmerCount=0
    WriteMasterFile
    EXIT FUNCTION
END IF
OPEN $MasterFile FOR INPUT AS #lFile
i = LOF(#lFile) \ SIZEOF(FarmerData(0))
IF (i>1) THEN       ' there are records

    LINE INPUT #lFile,s1
    TYPE SET FarmerData(0)=s1
    lFarmerCount=VAL(FarmerData(0).sIDnumber)
    FOR i=1 TO lFarmerCount         ' read all farmer data
        LINE INPUT #lFile,s1
        TYPE SET FarmerData(i)=s1
    NEXT i

    CLOSE #lFile
ELSE
    lFarmerCount=0
   FarmerData(0).sIDnumber =STR$(lFarmerCount)
    s1=FarmerData(0)
    PRINT #lFile,s1
    CLOSE #lFIle
END IF
END FUNCTION
'-----------------------------------------------------------------------------

'------------------------------------------------------------------------------
FUNCTION WriteMasterFile() AS LONG
LOCAL i,j,k   AS LONG
LOCAL    lFile   AS LONG
LOCAL s1,s2,s3, sFile AS STRING

lFile=FREEFILE

IF ISFILE($MasterFileBackup1) THEN
    FILECOPY $MasterFileBackup1,$MasterFileBackup2
END IF

IF ISFILE($MasterFile) THEN
    FILECOPY $MasterFile,$MasterFileBackup1
END IF

OPEN $MasterFile FOR OUTPUT AS #lFile
FarmerData(0).sIDnumber =STR$(lFarmerCount)
s1=FarmerData(0)
PRINT #lFile,s1
FOR i=1 TO lFarmerCount
    s1=FarmerData(i)
    PRINT #lFile,s1
NEXT i
CLOSE #lFIle

END FUNCTION
'-----------------------------------------------------------------------------
'------------------------------------------------------------------------------
FUNCTION ReadTransactionFile() AS LONG
LOCAL i,j,k   AS LONG
LOCAL    lFile   AS LONG
LOCAL s1,s2,s3, sFile AS STRING

lFile=FREEFILE

IF ISFILE($TransactionFIle)=0 THEN ' file not present
    lFarmerCount=0
    WriteTransactionFile
    EXIT FUNCTION
END IF

OPEN $TransactionFile FOR INPUT AS #lFile
i = LOF(#lFile) \ SIZEOF(TransactioNData(0))
IF (i>1) THEN       ' there are records

    IF i>1000 THEN
         REDIM PRESERVE TransactionData(i+10)
    END IF

    LINE INPUT #lFile,s1
    TYPE SET TransactionData(0)=s1
    lTransactionCount=VAL(TransactionData(0).sIDnumber)
    FOR i=1 TO lTransactionCount         ' read all farmer data
        LINE INPUT #lFile,s1
        TYPE SET TransactionData(i)=s1
    NEXT i

    CLOSE #lFile
ELSE
    lTransactionCount=0
   TransactionData(0).sIDnumber =STR$(lTransactionCount)
    s1=TransactionData(0)
    PRINT #lFile,s1
    CLOSE #lFIle
END IF
END FUNCTION
'-----------------------------------------------------------------------------
FUNCTION WriteTransactionFile() AS LONG
LOCAL i,j,k   AS LONG
LOCAL    lFile   AS LONG
LOCAL s1,s2,s3, sFile AS STRING

lFile=FREEFILE

IF ISFILE($TransactionFileBackup1) THEN
    FILECOPY $TransactionFileBackup1,$TransactionFileBackup2
END IF

IF ISFILE($TransactionFile) THEN
    FILECOPY $TransactionFile,$TransactionFileBackup1
END IF

OPEN $TransactionFile FOR OUTPUT AS #lFile
TransactionData(0).sIDnumber =STR$(lTransactionCount)
s1=TransactionData(0)
PRINT #lFile,s1
FOR i=1 TO lTransactionCount
    s1=TransactionData(i)
    PRINT #lFile,s1
NEXT i
CLOSE #lFIle

END FUNCTION
'------------------------------------------------------------------------------
