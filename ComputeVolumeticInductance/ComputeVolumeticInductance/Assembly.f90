    module AssemblyClass
    implicit none
    
    ! 振動体クラス
    type Assembly
        character*32 partName
        integer numofTimes, numofElements, numofNodes                   ! numofNodesはelementsの最大値
        double precision, dimension(:,:,:), allocatable :: positions    ! パート単位でレポートの座標値; 座標番号，時間番号，節点番号
        double precision, dimension(:,:,:), allocatable :: centroids    ! 重心
        double precision, dimension(:,:), allocatable :: volumes        ! 体積
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
    subroutine SetCentroidAndVolume(centroids, volumes, numofElements, elements, numofTimes, numofNodes, positions)
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
    type(Assembly) function init_Assembly(report, input, com) result(this)
        USE InputFileClass
        USE CommonReportClass
        USE ReportFileClass
        
        implicit none
        type(ReportFile), intent(in) :: report
        type(InputFile), intent(in) :: input
        type(CommonReport), intent(in) :: com
        
        integer ni, ti
        
        this%partName = report%partName
        this%numofTimes = com%numofTimes
        this%numofNodes = input%maximumNodeId       ! 要素節点として登録されている最大値を代入
        this%numofElements = input%numofElements
        
        ALLOCATE (this%positions(3, this%numofTimes, this%numofNodes))
        
        ! ローカル座標を足し合わせる処理
        do ni = 1, input%maximumNodeId
            do ti = 1, com%numofTimes
                this%positions(:,ti,ni) = report%positions(:,ti,ni) + input%localPosition
            end do
        end do
        
        ALLOCATE (this%centroids(3,this%numofTimes,this%numofElements))
        ALLOCATE (this%volumes(this%numofTimes,this%numofElements))
        
        ! 重心をここで設定
        CALL SetCentroidAndVolume(this%centroids, this%volumes, this%numofElements, input%elements, com%numofTimes, input%maximumNodeId, this%positions)
    end function
    
    end module