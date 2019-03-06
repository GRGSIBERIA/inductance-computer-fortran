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
            integer, dimension(:), allocatable :: wireFDs, coilFDs, coilVecFDs
            
            character*64 targetPart
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
            integer numofXYs, numofCoils
            integer FD, xyid, coilid, coilvecid
            
            numofXYs = 0
            numofCoils = 0
            xyid = 1
            coilid = 1
            coilvecid = 1
            FD = startFD
            
            this%configFD = FD
            OPEN (this%configFD, file=configPath, status="old")
            FD = FD + 1
            
            ! コイルやXYファイルの個数を決める
            DO
                READ (this%configFD, *, end=100) option, readData
                
                IF (INDEX(option, "wirefile") > 0) THEN
                    numofXYs = numofXYs + 1
                ELSE IF (INDEX(option, "coilfile") > 0) THEN
                    numofCoils = numofCoils + 1
                END IF
                
            END DO
100         continue
            
            ! ファイルの個数が決まったら領域を確保する
            ALLOCATE (this%wireFDs(numofXYs))
            ALLOCATE (this%coilFDs(numofCoils))
            ALLOCATE (this%coilVecFDs(numofCoils))
            this%numofWires = numofXYs
            this%numofCoils = numofCoils
            REWIND (this%configFD)
            
            ! 各ファイルを開く処理
            DO
                READ (this%configFD, *, end=200) option, readData
                    
                IF (INDEX(option, "wirefile") > 0) THEN
                    CALL ReadFile(this%wireFDs, xyid, FD, readData)
                ELSE IF (INDEX(option, "coilfile") > 0) THEN
                    CALL ReadFile(this%coilFDs, coilid, FD, readData)
                ELSE IF (INDEX(option, "coilvec") > 0) THEN
                    CALL ReadFile(this%coilVecFDs, coilvecid, FD, readData)
                ELSE IF (INDEX(option, "inpfile") > 0) THEN
                    this%inputFD = FD
                    OPEN (this%inputFD, file=readData, status="old")
                    FD = FD + 1
                ELSE IF (INDEX(option, "outfile") > 0) THEN
                    this%outputFD = FD
                    OPEN (this%outputFD, file=readData, status="replace")
                    FD = FD + 1
                ELSE IF (INDEX(option, "part") > 0) THEN
                    this%targetPart = readData
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
                CLOSE (this%coilVecFDs(i))
            END DO
            CLOSE (this%inputFD)
            CLOSE (this%configFD)
            CLOSE (this%outputFD)
            
            DEALLOCATE (this%wireFDs)
            DEALLOCATE (this%coilFDs)
            DEALLOCATE (this%coilVecFDs)
        end subroutine

    
    
    end module