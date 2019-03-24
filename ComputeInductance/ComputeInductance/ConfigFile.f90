    module ConfigClass
        implicit none
        
        type Config
            integer :: numofCoils, numofDRadius, numofDTheta
            integer :: configFD, inputFD, outputFD, nifFD
            integer, dimension(:), allocatable :: wireFDs, topFDs, bottomFDs
            character*32, dimension(:), allocatable :: wirePartNames, coilPartNames
            character*256 :: nifPath, configPath, inputPath, outputPath
            
        contains
            procedure :: Release => Config_Release
        end type
        
        interface Config
            module procedure :: init_Config
        end interface
        
    contains
    
    subroutine Config_Release(this)
        class(Config) :: this
        integer i
        CLOSE (this%configFD)
        CLOSE (this%inputFD)
        CLOSE (this%outputFD)
        CLOSE (this%nifFD)
        DO i = 1, SIZE(this%wireFDs)
            CLOSE (this%wireFDs(i))
        END DO
        DO i = 1, this%numofCoils
            CLOSE (this%topFDs(i))
            CLOSE (this%bottomFDs(i))
        END DO
        DEALLOCATE (this%topFDs)
        DEALLOCATE (this%bottomFDs)
        DEALLOCATE (this%wireFDs)
        DEALLOCATE (this%wirePartNames)
        DEALLOCATE (this%coilPartNames)
    end subroutine
    
    type(Config) function init_Config(startFD, configPath) result(this)
        implicit none
        character(*), intent(in) :: configPath
        integer startFD
            
        character*32    option, partname
        character*256   param, topfile, bottomfile, niffile, inpfile
        character*1024  line, also
        integer wireCount, coilCount, inputCount
        integer numofWires, numofCoils, numofInputs
            
        this%configFD = startFD
        this%configPath = configPath
        startFD = startFD + 1
            
            
        OPEN (this%configFD, file=configPath, status="old")
            
        numofInputs = 0
        numofWires = 0
        numofCoils = 0
            
        ! ファイルの数を数える
        do
            READ (this%configFD, *, end=100) option, param
                
            if (INDEX(option, "wirefile") > 0) then
                numofWires = numofWires + 1
            elseif (INDEX(option, "coilfile") > 0) then
                numofCoils = numofCoils + 1
            elseif (INDEX(option, "inpfile") > 0) then
                numofInputs = numofInputs + 1
            end if
        end do
100         continue
            
        ! ここからエラー処理
        if (numofInputs > 1) then
            PRINT *, "Please declare only one input file."
            PRINT *, "The number of inputfiles > ", numofInputs
        elseif (numofInputs <= 0) then
            PRINT *, "Please declare an inputfile."
            STOP
        end if
        ! エラー処理ここまで
            
        this%numofCoils = numofCoils
        ALLOCATE (this%wireFDs(numofWires))
        ALLOCATE (this%topFDs(numofCoils))
        ALLOCATE (this%bottomFDs(numofCoils))
        ALLOCATE (this%wirePartNames(numofWires))
        ALLOCATE (this%coilPartNames(numofCoils))
            
        ! ファイルパスを読み込んでコンフィグに記帳する
            
        ! wirefile, part名, XYデータ
        ! coilfile, part名, 上面XYデータ, 下面XYデータ
            
        REWIND (this%configFD)
            
        coilCount = 1
        wireCount = 1
        inputCount = 1
            
        do
            READ (this%configFD, "(A)", end=200) line
                
            if (LEN_TRIM(line) < 1) then
                goto 200    ! 空白行があるときはファイルの末尾に移動したと判断する
            end if
                
            READ (line, *) option, also
                
            if (INDEX(option, "wirefile") > 0) then     ! ワイヤの読み込み
                READ (line, *) option, partname, param
                OPEN (startFD, file=param, status="old")
                this%wireFDs(wireCount) = startFD
                this%wirePartNames(wireCount) = partname
                wireCount = wireCount + 1
                startFD = startFD + 1
                    
            elseif(INDEX(option, "coilfile") > 0) then  ! 上面，下面の順番で記帳
                READ (line, *) option, partname, topfile, bottomfile
                OPEN (startFD, file=topfile, status="old")
                OPEN (startFD+1, file=bottomfile, status="old")
                this%coilPartNames(coilCount) = partname
                this%topFDs(coilCount) = startFD
                this%bottomFDs(coilCount) = startFD + 1
                coilCount = coilCount + 1
                startFD = startFD + 2
                    
            elseif(INDEX(option, "inpfile") > 0) then ! 入力ファイルの読み込み
                READ (line, *) option, param
                OPEN (startFD, file=param, status="old")
                this%inputFD = startFD
                this%inputPath = param
                startFD = startFD + 1
                
            elseif(INDEX(option, "outputfile") > 0) then ! 出力ファイルを開く
                READ (line, *) option, param
                OPEN (startFD, file=param, status="replace")
                this%outputFD = startFD
                this%outputPath = param
                startFD = startFD + 1
                
            elseif(INDEX(option, "niffile") > 0) then
                READ (line, *) option, param
                OPEN (startFD, file=param, status="old")
                this%nifPath = param
                this%nifFD = startFD
                startFD = startFD + 1
                    
            elseif(INDEX(option, "dradius") > 0) then
                READ (line, *) option, this%numofDRadius
                    
            elseif(INDEX(option, "dtheta") > 0) then
                READ (line, *) option, this%numofDTheta
                    
            end if
                
        end do
200     continue            
        
    end function
    
    
    end module
    