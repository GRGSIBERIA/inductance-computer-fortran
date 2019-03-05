    module Report
    use AssemblyClass
    use XYDataClass
    implicit none
    
    contains
    
    integer function CountTimes(xyFD) result(numofTimes)
        implicit none
        integer, intent(in) :: xyFD
        character*64 capture
    
        ! 時系列データが格納されている
        numofTimes = 0
        DO
            READ (xyFD, "(A)", end=3100) capture
            
            IF (INDEX(capture, ".") == 0) THEN
                GOTO 3100
            END IF
            
            numofTimes = numofTimes + 1
        END DO
3100    continue
        
    end function
    
    
    ! ヘッダ情報を読み取るサブルーチン
    subroutine ReadHeader(xyFD, nodeNumber, axisNumber, status)
        implicit none
        integer, intent(in) :: xyFD
        integer, intent(out) :: nodeNumber, axisNumber, status
        character*64 capture, header
        
        ! 1行目には値の情報が含まれている
        READ (xyFD, "(A)", end=3000) header
        
        ! 節点の座標系データが含まれていなければデータが終了しているので例外としてGOTOする
        IF (INDEX(header, "U:U") == 0) THEN
            status = -1
            GOTO 3001
        END IF
        
        ! 2行目以降は連続的に読み込む
        DO
            READ (xyFD, "(A)", end=3000) capture
            IF (INDEX(capture, "  X  ") > 0) THEN
                GOTO 3000       ! ヘッダの終わりにX軸のラベルが存在する
            END IF
        END DO
3000    continue
        
        ! ヘッダの中で必要な情報だけ読み取る
        READ (header(INDEX(header, "U:U")+3:), *) axisNumber
        READ (capture(INDEX(capture, "N: ")+3:), "(I)") nodeNumber
3001    continue        
    end subroutine
    
    
    ! 時間の数を読み取る関数
    integer function ReadNumberOfTimes(xyFD) result(numofTimes)
        implicit none
        integer, intent(in) :: xyFD
        integer nodeNumber, axisNumber, status
        
        READ (xyFD, "()")   ! 先頭行のスキップ
        CALL ReadHeader(xyFD, nodeNumber, axisNumber, status)   ! ヘッダ行の読み取り
        READ (xyFD, "()")   ! 1行読み捨てる
        
        ! 時間だけ取得して最初に戻す
        numofTimes = CountTimes(xyFD)
        REWIND (xyFD)
    end function
    
    
    integer function ReadNumberOfData(xyFD, numofTimes) result(numofData)
        implicit none
        integer, intent(in) :: xyFD, numofTimes
        integer nodeNumber, axisNumber, status
        integer i
        
        numofData = 0
        DO
            READ (xyFD, "()", end=3200)
            CALL ReadHeader(xyFD, nodeNumber, axisNumber, status)
            IF (status == -1) THEN
                REWIND (xyFD)
                GOTO 3200   ! データの終わりを通知されたので終了する
            END IF
            
            READ (xyFD, "()", end=3200)
            
            DO i = 1, numofTimes
                READ (xyFD, "()")
            END DO
            
            ! 全部リードが終わったらデータを追記
            numofData = numofData + 1
            
            READ (xyFD, "()", end=3200)
            READ (xyFD, "()", end=3200)
        END DO
3200    continue
        
    end function
    
    
    ! 時間のデータだけ読み込む
    subroutine ReadTimeData(xyFD, numofTimes, times)
        implicit none
        integer, intent(in) :: xyFD, numofTimes
        real, dimension(numofTimes), intent(out) :: times
        integer nodeNumber, axisNumber, status, i
        real time, displace
        
        READ (xyFD, "()")
        CALL ReadHeader(xyFD, nodeNumber, axisNumber, status)
        READ (xyFD, "()")
        
        ! ここで時間を読み込む
        DO i = 1, numofTimes
            READ (xyFD, *) times(i), displace
        END DO
        REWIND (xyFD)
        
        PRINT *, "Complete Read Times: ", numofTimes
    end subroutine
    
    
    ! ノード番号を読み込む
    subroutine ReadNodeIds(xyFD, numofTimes, numofData, nodeIds)
        implicit none
        integer, intent(in) :: xyFD, numofTimes, numofData
        integer, dimension(:), intent(out) :: nodeIds
        
        integer tid, did
        integer nodeNumber, axisNumber, status
        real time
        
        DO did = 1, numofData
            ! ヘッダの読み込み
            READ (xyFD, "()")
            CALL ReadHeader(xyFD, nodeIds(did), axisNumber, status) ! ここでノード番号を記入
            READ (xyFD, "()")

            DO tid = 1, numofTimes
                READ (xyFD, "()")
            END DO
            
            READ (xyFD, "()")
            READ (xyFD, "()")
        END DO
        REWIND (xyFD)
        
        PRINT *, "Complete Read Data: ", numofData
        PRINT *, "Number of Nodes: ", numofData / 3
    end subroutine
    
    
    ! 未ソートの座標データをすべて読み込む
    subroutine ReadDisplaces(xyFD, numofTimes, numofData, unsortDisplaces)
        implicit none
        integer, intent(in) :: xyFD, numofTimes, numofData
        real, dimension(:,:,:), intent(out) :: unsortDisplaces
        
        integer tid, did, nid
        integer nodeNumber, axisNumber, status
        real time
        
        DO did = 1, numofData
            READ (xyFD, "()")
            CALL ReadHeader(xyFD, nodeNumber, axisNumber, status)
            READ (xyFD, "()")
            
            DO tid = 1, numofTimes
                READ (xyFD, *) time, unsortDisplaces(tid, did, axisNumber)
            END DO
            
            READ (xyFD, "()")
            READ (xyFD, "()")
        END DO
        
    end subroutine
    
    
    
    ! レポートファイルを読み込む手続き
    type(XYData) function LoadReport(xyFD, asm) result(xydata)
        implicit none
        integer, intent(in) :: xyFD
        class(Assembly) :: asm
        
        integer nodeNumber, axisNumber
        integer numofTimes, numofData
        
        
        ! データの準備
        numofTimes = ReadNumberOfTimes(xyFD)
        numofData = ReadNumberOfData(xyFD, numofTimes)
        xydata = init_XYData(numofTimes, numofData)
        
        CALL ReadTimeData(xyFD, numofTimes, xydata%times)
        CALL ReadNodeIds(xyFD, numofTimes, numofData, xydata%nodeIds)
        CALL ReadDisplaces(xyFD, numofTimes, numofData, xydata%unsortDisplaces)
        
        CALL ConstructDisplaces(xydata, asm)
    end function
    
    end module