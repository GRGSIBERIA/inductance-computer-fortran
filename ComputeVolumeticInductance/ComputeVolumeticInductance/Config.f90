    module ConfigClass
    
    type Config
        integer :: inputFD
        integer :: numofParts, numofCoils
        integer, dimension(:), allocatable :: partFDs
        integer, dimension(:), allocatable :: coilTopFDs
        integer, dimension(:), allocatable :: coilBottomFDs
        
        character*32, dimension(:), allocatable :: partNames
        character*32, dimension(:), allocatable :: coilNames
    contains
        procedure :: Release => Release_Config
    end type
    
    contains
    
    subroutine FixSizes(fd, numofParts, numofCoils)
        implicit none
        integer, intent(in) :: fd
        integer, intent(out) :: numofParts, numofCoils
        character*256 line
        
        numofParts = 0
        numofCoils = 0
        do
            READ (fd, "(A)", end=100) line
            if (INDEX(line, "*part") == 1) then
                numofParts = numofParts + 1
            else if (INDEX(line, "*coil") == 1) then
                numofCoils = numofCoils + 1
            end if
        end do
100     continue
    end subroutine
    
    type(Config) function init_Config(path) result(this)
        implicit none
        character*128, intent(in) :: path
        integer, parameter :: configFD = 20
        character*256 line
        
        OPEN (configFD, file=path, status="old")
        
        ! ex.
        ! *inpfile, E:/hogehoge.inp
        ! *part, tine, E:/tine.rpt
        ! *part, tonebar, E:/tonebar.rpt
        ! *coil, coil-1, E:/coil-1-top.rpt, E:/coil-1-btm.rpt
        ! *coil, coil-2, E:/coil-2-top.rpt, E:/coil-2-btm.rpt
        
        ! *inpfile, inpファイルのパス
        !   inpファイルを読み込んでデータに起こす
        ! *part, パート名, rptファイルのパス
        !   使用するパートを記述し，それに対応するrptファイルを渡す
        ! *coil, パート名, コイル上面のrptファイルのパス, コイル底面のrptファイルのパス
        !   使用するcoilのパートを記述し，それに対応するコイル上面と底面のrptファイルを渡す
        
        ! 配列の大きさを確定
        CALL FixSizes(configFD, this%numofParts, this%numofCoils)
        
        ! 要素が確定したので配列を確保
        ALLOCATE (this%partFDs(this%numofParts))
        ALLOCATE (this%coilTopFDs(this%numofCoils))
        ALLOCATE (this%coilBottomFDs(this%numofCoils))
        ALLOCATE (this%partNames(this%numofParts))
        ALLOCATE (this%coilNames(this%numofCoils))
        
        REWIND (configFD)
        
        do
            
        end do
200     continue        
        
        CLOSE (configFD)
    end function
    
    
    ! リソースの解放処理
    subroutine Release_Config(this)
        implicit none
        class(Config) this
        integer i
        
        CLOSE (this%inputFD)
        do i = 1, SIZE(this%reportFDs)
            CLOSE (this%reportFDs(i))
        end do
        
        DEALLOCATE (this%partFDs)
        DEALLOCATE (this%coilTopFDs)
        DEALLOCATE (this%coilBottomFDs)
        DEALLOCATE (this%partNames)
        DEALLOCATE (this%coilNames)
    end subroutine
    
    end module