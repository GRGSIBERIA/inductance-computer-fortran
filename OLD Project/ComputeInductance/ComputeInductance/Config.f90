    module ConfigClass
        implicit none
        
        ! メモ
        ! 非透磁率
        ! 鉄: 5000
        ! ニッケル: 100～600
    
        ! gammaを決定するなら，
        ! 鉄を1としたとき
        ! ニッケルは0.1程度
        
        ! 設定ファイルを扱う構造体
        type Config
            integer numofWires, numofCoils
            integer configFD, inputFD, outputFD
            integer, dimension(:), allocatable :: wireFDs, coilFDs
            
            character*64 wirePart
            character*64 coilPart
            
            integer, dimension(:), allocatable :: coilVectorFrontIds, coilVectorBackIds
            real, dimension(:), allocatable :: coilHeights
            real, dimension(:), allocatable :: coilRadius
        contains
            procedure :: Release => Config_Release
        end type
        
        interface Config
            module procedure :: init_Config
        end interface
        
    contains
        ! ファイルを読み込んでFDなどをインクリメントする
        subroutine ReadFile(fds, id, FD ,readData)
            implicit none
            character*256, intent(in) :: readData
            integer, dimension(:), intent(inout) :: fds
            integer, intent(inout) :: id, FD
            
            fds(id) = FD
            OPEN (fds(id), file=readData, status="old")
            FD = FD + 1
            id = id + 1
        end subroutine
    
        type(Config) function init_Config(startFD, configPath) result(this)
            implicit none
            character*256, intent(in) :: configPath
            integer, intent(in) :: startFD
            
            character*16    :: option
            character*256   :: readData
            integer FD, xyid, coilid, front, back
            real height, radius
            
            this%numofWires = 0
            this%numofCoils = 0
            xyid = 1
            coilid = 1
            FD = startFD
            
            this%configFD = FD
            OPEN (this%configFD, file=configPath, status="old")
            FD = FD + 1
            
            ! コイルやXYファイルの個数を決める
            DO
                READ (this%configFD, *, end=100) option, readData
                
                IF (INDEX(option, "wirefile") > 0) THEN
                    this%numofWires = this%numofWires + 1
                ELSE IF (INDEX(option, "coilfile") > 0) THEN
                    this%numofCoils = this%numofCoils + 1
                END IF
                
            END DO
100         continue
            
            ! ファイルの個数が決まったら領域を確保する
            ALLOCATE (this%wireFDs(this%numofWires))
            ALLOCATE (this%coilFDs(this%numofCoils))
            ALLOCATE (this%coilVectorFrontIds(this%numofCoils))
            ALLOCATE (this%coilVectorBackIds(this%numofCoils))
            ALLOCATE (this%coilHeights(this%numofCoils))
            ALLOCATE (this%coilRadius(this%numofCoils))
            REWIND (this%configFD)
            
            ! 各ファイルを開く処理
            DO
                READ (this%configFD, *, end=200) option, readData
                    
                IF (INDEX(option, "wirefile") > 0) THEN
                    CALL ReadFile(this%wireFDs, xyid, FD, readData)
                    
                    READ (this%configFD, *, end=200) option, readData   ! wirefileのあとにwirepartが続く
                    IF (INDEX(option, "wirepart") <= 0) THEN
                        PRINT *, "Please continue wirepart after wirefile."
                        STOP
                    END IF
                    
                    this%wirePart = readData        ! ファイルのあとに必ずpart名
                    
                ELSE IF (INDEX(option, "coilfile") > 0) THEN
                    CALL ReadFile(this%coilFDs, coilid, FD, readData)
                    
                    READ (this%configFD, *, end=200) option, readData   ! coilfileのあとにcoilpartが続く
                    IF (INDEX(option, "coilpart") <= 0) THEN
                        PRINT *, "Please continue coilpart after coilfile."
                        STOP
                    END IF
                    
                    this%coilPart = readData
                    
                    ! コイルの向きを決めるノードIDを保存する
                    READ (this%configFD, *, end=200) option, front, back                ! coilpartのあとにnodeidが続く
                    IF (INDEX(option, "nodeid") <= 0) THEN
                        PRINT *, "Please continue nodeid after coilpart."
                        STOP
                    END IF
                    this%coilVectorFrontIds(coilid-1) = front
                    this%coilVectorBackIds(coilid-1) = back
                    
                    READ (this%configFD, *, end=200) option, height, radius             ! nodeidのあとにcoildataが続く
                    IF (INDEX(option, "coildata") <= 0) THEN
                        PRINT *, "Please continue coildata after nodeid."
                        STOP
                    END IF
                    this%coilHeights(coilid-1) = height
                    this%coilRadius(coilid-1) = radius
                    
                ELSE IF (INDEX(option, "inpfile") > 0) THEN
                    this%inputFD = FD
                    OPEN (this%inputFD, file=readData, status="old")
                    FD = FD + 1
                    
                ELSE IF (INDEX(option, "outfile") > 0) THEN
                    this%outputFD = FD
                    OPEN (this%outputFD, file=readData, status="replace")
                    FD = FD + 1
                    
                END IF
                
            END DO
200         continue
            
            REWIND (this%configFD)
        end function
    
        ! ファイルを閉じてリソースを解放する
        subroutine Config_Release(this)
            class(Config) this
            integer i
            
            DO i = 1, SIZE(this%wireFDs)
                CLOSE (this%wireFDs(i))
            END DO
            DO i = 1, SIZE(this%coilFDs)
                CLOSE (this%coilFDs(i))
            END DO
            CLOSE (this%inputFD)
            CLOSE (this%configFD)
            CLOSE (this%outputFD)
            
            DEALLOCATE (this%wireFDs)
            DEALLOCATE (this%coilFDs)
            DEALLOCATE (this%coilVectorFrontIds)
            DEALLOCATE (this%coilVectorBackIds)
            DEALLOCATE (this%coilRadius)
            DEALLOCATE (this%coilHeights)
        end subroutine

    
    
    end module