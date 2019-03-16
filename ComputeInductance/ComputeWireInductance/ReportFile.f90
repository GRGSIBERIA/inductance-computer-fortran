    module ReportFileClass
    implicit none
    
    type ReportFile
        integer, dimension(:), allocatable :: nodeIds
        real, dimension(:), allocatable :: times
        real, dimension(:,:,:), allocatable :: positions
    contains
        procedure :: PrintInformation => ReportFile_PrintInformation
    end type
    
    contains
    
    subroutine ReportFile_PrintInformation(this, identifier)
        class(ReportFile) this
        character(*), intent(in) :: identifier
        
        PRINT *, "---------------------------------------------"
        PRINT *, identifier
        PRINT *, "Number of Times: ", SIZE(this%times)
        PRINT *, "Number of Nodes: ", SIZE(this%nodeIds)
    end subroutine
    
    type(ReportFile) function init_ReportFile(reportFD, input) result(this)
        use InputFileClass
        implicit none
        integer, intent(in) :: reportFD
        type(InputFile) input
        
        character*128 line, header
        integer numofData, numofNodes, numofTimes, i
        integer axisPos, axisid, headerPos, nodePos, nodeid, timeid
        integer countNodes
        real time
        
        ! preparateFlagsを使って全くデータの代入が起きていない節点を探索する
        integer, dimension(:), allocatable :: preparateFlags
        real, dimension(:,:,:), allocatable :: preparatePositions
        
        READ (reportFD, "()")   ! 先頭1行飛ばす
        
        numofData = 0
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! データ件数を調べる
        do
            READ (reportFD, "(A)", end=100) line
            
            if (INDEX(line, "  X  ") > 0) then
                numofData = numofData + 1
            end if
            
        end do
100     continue
        
        ! 余りが0じゃなければ不正と見てプログラムを停止する
        if (MOD(numofData, 3) /= 0) then
            PRINT *, reportFD, "is not mod(numofData, 3) == 0."
            STOP
        end if
        
        numofNodes = numofData / 3
        PRINT *, "number of nodes:", numofNodes
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 時間の件数を調べる
        REWIND (reportFD)
        
        ! 先頭4行はなにもない
        do i = 1, 4
            READ (reportFD, "()")
        end do
        
        ! 時間のカウント
        numofTimes = 0
        do
            READ (reportFD, "(A)") line
            
            if (LEN_TRIM(line) == 0) then
                goto 200    ! 空の行が来たら脱出する
            end if
            
            numofTimes = numofTimes + 1
        end do
200     continue
        
        PRINT *, "number of times:", numofTimes
        
        ALLOCATE (this%positions(numofTimes, numofNodes, 3))
        ALLOCATE (this%times(numofTimes))
        ALLOCATE (this%nodeIds(numofNodes))
        ALLOCATE (preparatePositions(numofTimes, SIZE(input%nodeIds), 3))
        ALLOCATE (preparateFlags(SIZE(input%nodeIds)))
        
        this%positions = 0
        this%times = 0
        preparatePositions = 0
        preparateFlags = 0
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 時系列データのロード
        REWIND (reportFD)
        
        do
            READ (reportFD, "()")   ! 空行のスキップ
            
            ! ヘッダ行の読み込み
            READ (reportFD, "(A)") line
            if (LEN_TRIM(line) == 0) then
                goto 600    ! ヘッダ行が存在しない場合はデータの読み込みが終わったと判断する
            end if
                
            ! 最初に軸IDを取得
            axisPos = INDEX(line, "U:U") + 3
            READ (line(axisPos:axisPos), *) axisid
                
            ! ヘッダ行の生成
            header = ""
            do
                headerPos = INDEX(line, "  X  ")
                if (headerPos > 0) then  ! X軸フィールドが含まれていたら終わり
                    header = TRIM(header) // ADJUSTL(line(headerPos+5:))
                    goto 400
                end if
                    
                header = TRIM(header) // ADJUSTL(line)
                READ (reportFD, "(A)") line
            end do
400         continue
                
            ! ノード番号を取得
            nodePos = INDEX(header, "N: ")
            READ (header(nodePos+3:), *) nodeid
            preparateFlags(nodeid) = preparateFlags(nodeid) + 1 ! ノード番号が見つかったのでフラグに1を足す
                
            READ (reportFD, "()")   ! 1行読み飛ばし
            
            ! 時系列データのスタート
            ! nodeid, axisidは準備できているのでInputFileから参照してデータを挿入していく
            timeid = 1
            do
                READ (reportFD, "(A)") line
                
                if (LEN_TRIM(line) == 0) then
                    goto 500
                end if
                
                READ (line, *) this%times(timeid), preparatePositions(timeid, nodeid, axisid)
                timeid = timeid + 1
            end do
500         continue
            
            READ (reportFD, "()")   ! 次のデータが入るかもしれないので2行飛ばす
        end do
600     continue
        
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! 読み込み済みデータの整理
        countNodes = 1
        do nodeid = 1, SIZE(input%nodeIds)
            if (preparateFlags(nodeid) > 0) then    ! フラグが立っていたら整理
                
                do timeid = 1, numofTimes
                    this%positions(timeid, countNodes, :) = preparatePositions(timeid, nodeid, :) + input%positions(nodeid, :)
                end do
                
                this%nodeIds(countNodes) = nodeid   ! inputFileを参照しているノード番号を代入
                countNodes = countNodes + 1
            end if
        end do
        
    end function
    
    end module
    