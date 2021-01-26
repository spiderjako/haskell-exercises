type Fn = String
type FirstName = String
type LastName = String
type Score = Float
type DisciplineID = String
type Course = Integer
type DisciplineName = String
type CourseYear = Integer

type StudentScore = (Fn, Score, DisciplineID)
type Student = (FirstName, LastName, Fn, Course)
type Discipline = (DisciplineName, DisciplineID, CourseYear)

getStudentScoreFnAndDisciplineID :: StudentScore -> (Fn, DisciplineID)
getStudentScoreFnAndDisciplineID (a,_,c)  = (a,c)

getStudentFirstLastName :: Student -> (FirstName, LastName)
getStudentFirstLastName (a, b, _, _) = (a, b)
getStudentFn :: Student -> Fn
getStudentFn (_, _, c, _) = c
getStudentCourse :: Student -> Course
getStudentCourse (_,_,_,d) = d

getStudentScoreFn :: StudentScore -> Fn
getStudentScoreFn (a, _, _) = a
getStudentScoreScore :: StudentScore -> Score
getStudentScoreScore(_,b,_) = b
getStudentScoreDisciplineID :: StudentScore -> DisciplineID
getStudentScoreDisciplineID (_, _, c) = c

getDisciplineDisciplineName :: Discipline -> DisciplineName
getDisciplineDisciplineName (a,_,_) = a
getDisciplineDisciplineID :: Discipline -> DisciplineID
getDisciplineDisciplineID (_,b,_) = b
getDisciplineCourseYear :: Discipline -> CourseYear
getDisciplineCourseYear (_,_,c) = c

filterScoresByDiscipline :: [StudentScore] -> DisciplineID -> [StudentScore]
filterScoresByDiscipline scores disciplineID =
    filter (\x -> getStudentScoreDisciplineID x == disciplineID) scores

getNamesByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> [(FirstName, LastName)]
getNamesByDisciplineID scores students disciplineId =
    let filteredStudentScores = filter (\x -> getStudentScoreDisciplineID x == disciplineId) scores
        fnInDiscipline = map getStudentScoreFn filteredStudentScores
        filteredStudents = filter (\x -> getStudentFn x `elem` fnInDiscipline) students
    in map getStudentFirstLastName filteredStudents

calculateAvgGrade :: Discipline -> [StudentScore] -> Score
calculateAvgGrade discipline filteredStudentScores =
    let groupedScores = map getStudentScoreScore (filterScoresByDiscipline filteredStudentScores (getDisciplineDisciplineID discipline))
    in sum groupedScores / (fromIntegral (length groupedScores))

getResults :: [StudentScore] -> [Discipline] -> [(DisciplineName, DisciplineID, Score)]
getResults scores disciplines =
    let listOfDisciplineIDs = map getDisciplineDisciplineID disciplines
        filteredStudentScores = filter (\x -> getStudentScoreDisciplineID x `elem` listOfDisciplineIDs) scores
    in [
            (
                getDisciplineDisciplineName discipline,
                getDisciplineDisciplineID discipline,
                calculateAvgGrade discipline filteredStudentScores
            )  |
            discipline <- disciplines
        ]

getAverageGradesGrade :: (DisciplineName, DisciplineID, Score) -> Score
getAverageGradesGrade (_, _, grade) = grade

getAverageGradesDisciplineID :: (DisciplineName, DisciplineID, Score) -> DisciplineID
getAverageGradesDisciplineID (_, id, _) = id

getAboveAvеrageStudents :: [StudentScore] -> [Student] -> [Discipline] -> [Student]
getAboveAvеrageStudents scores students disciplines =
    let averageGradesForDisciplines = getResults scores disciplines
        filteredGrades = [
            [
                score
                | score <- scores,
                getStudentScoreDisciplineID score == getAverageGradesDisciplineID discipline,
                getAverageGradesGrade discipline < getStudentScoreScore score
            ] | discipline <- averageGradesForDisciplines]
    in [
            student
            | student <- students,
            all (\x -> (getDisciplineCourseYear (snd x) == getStudentCourse student)
                            && (any (\y -> getStudentFn student == getStudentScoreFn y) (fst x))) (zip filteredGrades disciplines)
        ]
