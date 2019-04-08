    module OscillatorClass
    implicit none
    
    ! 振動体クラス
    type Oscillator
        character*32 partName
        integer numofTimes, numofElements, numofNodes                   ! numofNodesはelementsの最大値
        double precision, dimension(:,:,:), allocatable :: positions    ! パート単位でレポートの座標値
        double precision, dimension(:,:,:), allocatable :: centroids     ! 重心
        double precision, dimension(:,:), allocatable :: volumes         ! 体積
    end type
    
    contains
    
    
    ! 体積を返す関数
    double precision function ComputeVolume(a,b,c,d) result(volume)
        USE Math
        implicit none
        double precision, dimension(3), intent(in) :: a, b, c, d
        volume = (Length(b-a) * Length(c-a) * Length(d-a)) / 3.0d0
    end function
    
    
    ! 重心を設定する関数
    subroutine SetCentroidAndVolume(centroid, volumes, numofElements, elements, numofTimes, numofNodes, positions)
        implicit none
        integer, intent(in) :: numofElements, numofTimes, numofNodes
        double precision, dimension(3,numofTimes,numofElements), intent(out) :: centroids
        double precision, dimension(numofTimes,numofElements), intent(out) :: volumes
        double precision, dimension(3,numofTimes,numofNodes), intent(in) :: positions
        integer, dimension(4,numofElements), intent(in) :: elements
        
        integer eid, vid, tid
        
        centroids = 0
        
        ! 重心を求めるために要素節点から節点の座標値を足し合わせる
        do eid = 1, numofElements
            do tid = 1, numofTimes    
                do vid = 1, 4
                    centroids(:,tid,eid) = centroids(:,tid,eid) + positions(:,tid,elements(vid,eid))
                end do
            end do
        end do
        
        do eid = 1, numofElements
            do tid = 1, numofTimes
                volumes(tid,eid) = ComputeVolume(&
                    positions(:,tid,elements(1,eid)), &
                    positions(:,tid,elements(2,eid)), &
                    positions(:,tid,elements(3,eid)), &
                    positions(:,tid,elements(4,eid)))
            end do
        end do
        
        ! 4点の平均を取って終わり
        centroids = centroids / 4
    end subroutine
    
    
    
    ! numofNodes は maximumNodeIdと同じ数を宣言している
    ! inputもパートごとに存在しているので取扱に注意する
    type(Oscillator) function init_Oscillator(part, reports, input, com) result(this)
        USE InputFileClass
        USE CommonReportClass
        USE ReportFileClass
        implicit none
        character*32, intent(in) :: part
        type(ReportFile), dimension(:), intent(in) :: reports
        type(InputFile), intent(in) :: input
        type(CommonReport), intent(in) :: com
        
        integer i
        
        this%partName = part
        ALLOCATE (this%positions(3,numofTimes, numofNodes))
        this%positions = 0
        this%numofTimes = com%numofTimes
        this%numofNodes = input%maximumNodeId       ! 要素節点として登録されている最大値を代入
        this%numofElements = input%numofElements
        
        do i = 1, SIZE(reports)
            if (part == reports(i)%partName .and. part == input%partName) then
                this%positions = this%positions + reports(i)%positions
            end if
        end do
        
        ALLOCATE (this%centroids(3,this%numofTimes,this%numofElements))
        ALLOCATE (this%volumes(this%numofTimes,this%numofElements))
        
        ! まだ重心は設定してないよ！
    end function
    
    end module