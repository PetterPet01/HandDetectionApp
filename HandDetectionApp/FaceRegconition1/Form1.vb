Imports Emgu.CV.Structure
Imports Emgu.CV
Imports Emgu.Util
Imports Emgu.CV.VideoSurveillance
Imports [Module].AForge.Extensions
Imports AForge.Imaging
Imports AForge.Imaging.Filters
Imports System.Drawing.Imaging
'Imports AForge

Public Class Form1
    Dim gray As Image(Of Gray, Byte) = Nothing
    Dim grabber As New Capture
    Dim currentFrame As Image(Of Bgr, [Byte])
    Dim processedFrame As Image(Of Bgr, [Byte])
    Dim face As HaarCascade
    Dim hand As HaarCascade
    Dim result As Image(Of Gray, Byte), TrainedFace As Image(Of Gray, Byte) = Nothing
    Dim defects As Seq(Of MCvConvexityDefect)
    Dim defectArray As MCvConvexityDefect()
    Dim box(1) As MCvBox2D


    Dim bc As Color
    Dim iColorToSet As Integer
    Dim CDHandPoints(1) As System.Drawing.Point
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        hand = New HaarCascade("Hand.Cascade.1 (2).xml")
        face = New HaarCascade("haarcascade_frontalface_default (2).xml")

    End Sub
    Public Shared Sub RgbToHls(ByVal r As Integer, ByVal g As Integer, ByVal b As Integer, ByRef h As Double, ByRef l As Double, ByRef s As Double)
        Dim double_r As Double = r / 255.0
        Dim double_g As Double = g / 255.0
        Dim double_b As Double = b / 255.0
        Dim max As Double = double_r
        If max < double_g Then max = double_g
        If max < double_b Then max = double_b
        Dim min As Double = double_r
        If min > double_g Then min = double_g
        If min > double_b Then min = double_b
        Dim diff As Double = max - min
        l = (max + min) / 2

        If Math.Abs(diff) < 0.00001 Then
            s = 0
            h = 0
        Else

            If l <= 0.5 Then
                s = diff / (max + min)
            Else
                s = diff / (2 - max - min)
            End If

            Dim r_dist As Double = (max - double_r) / diff
            Dim g_dist As Double = (max - double_g) / diff
            Dim b_dist As Double = (max - double_b) / diff

            If double_r = max Then
                h = b_dist - g_dist
            ElseIf double_g = max Then
                h = 2 + r_dist - b_dist
            Else
                h = 4 + g_dist - r_dist
            End If

            h = h * 60
            If h < 0 Then h += 360
        End If
    End Sub

    Private Function HlsToRgb(ByVal h As Double, ByVal l As Double, ByVal s As Double) As Color
        Dim p2 As Double

        If l <= 0.5 Then
            p2 = l * (1 + s)
        Else
            p2 = l + s - l * s
        End If

        Dim p1 As Double = 2 * l - p2
        Dim double_r, double_g, double_b As Double

        If s = 0 Then
            double_r = l
            double_g = l
            double_b = l
        Else
            double_r = QqhToRgb(p1, p2, h + 120)
            double_g = QqhToRgb(p1, p2, h)
            double_b = QqhToRgb(p1, p2, h - 120)
        End If

        Dim r = CInt((double_r * 255.0))
        Dim g = CInt((double_g * 255.0))
        Dim b = CInt((double_b * 255.0))
        Return Color.FromArgb(r, g, b)
    End Function

    Private Shared Function QqhToRgb(ByVal q1 As Double, ByVal q2 As Double, ByVal hue As Double) As Double
        If hue > 360 Then
            hue -= 360
        ElseIf hue < 0 Then
            hue += 360
        End If

        If hue < 60 Then Return q1 + (q2 - q1) * hue / 60
        If hue < 180 Then Return q2
        If hue < 240 Then Return q1 + (q2 - q1) * (240 - hue) / 60
        Return q1
    End Function

    Private Sub ExtractContourAndHull(ByRef skin As Image(Of Gray, Byte), index As Integer)
        Dim r As Rectangle
        Using storage As MemStorage = New MemStorage()
            Dim contours As Contour(Of Point) = skin.FindContours(Emgu.CV.CvEnum.CHAIN_APPROX_METHOD.CV_CHAIN_APPROX_SIMPLE, Emgu.CV.CvEnum.RETR_TYPE.CV_RETR_LIST, storage)
            Dim biggestContour As Contour(Of Point) = Nothing
            Dim Result1 As Double = 0
            Dim Result2 As Double = 0

            While contours IsNot Nothing
                Result1 = contours.Area

                If Result1 > Result2 Then
                    Result2 = Result1
                    biggestContour = contours
                End If

                contours = contours.HNext
            End While

            If biggestContour IsNot Nothing Then
                Dim currentContour As Contour(Of Point) = biggestContour.ApproxPoly(biggestContour.Perimeter * 0.0025, storage)
                currentFrame.Draw(currentContour, New Bgr(Color.LimeGreen), 2)
                biggestContour = currentContour
                Dim hull = biggestContour.GetConvexHull(Emgu.CV.CvEnum.ORIENTATION.CV_CLOCKWISE)
                box(index) = biggestContour.GetMinAreaRect()
                Dim points As PointF() = box(index).GetVertices()
                Dim ps = New Point(points.Length - 1) {}

                For i = 0 To points.Length - 1
                    ps(i) = New Point(CInt(points(i).X), CInt(points(i).Y))
                Next

                currentFrame.DrawPolyline(hull.ToArray(), True, New Bgr(200, 125, 75), 2)
                currentFrame.Draw(New CircleF(New PointF(box(index).center.X, box(index).center.Y), 3), New Bgr(200, 125, 75), 2)
                CDHandPoints(index) = New System.Drawing.Point(box(index).center.X, box(index).center.Y)
                r = New Rectangle(box(index).center.X - box(index).size.Width / 2, box(index).center.Y - box(index).size.Height / 2, box(index).size.Width, box(index).size.Height)


                Dim center As PointF
                Dim radius As Single
                Dim filteredHull = New Seq(Of Point)(storage)

                For i As Integer = 0 To hull.Total - 1

                    If Math.Sqrt(Math.Pow(hull(CInt(i)).X - hull(CInt(i + 1)).X, 2) + Math.Pow(hull(CInt(i)).Y - hull(CInt(i + 1)).Y, 2)) > box(index).size.Width / 10 Then
                        filteredHull.Push(hull(i))
                    End If
                Next

                defects = biggestContour.GetConvexityDefacts(storage, Emgu.CV.CvEnum.ORIENTATION.CV_CLOCKWISE)
                defectArray = defects.ToArray()
            End If
        End Using
        DrawAndComputeFingersNum(r, skin, index)
    End Sub
    Dim fingerNum = 0
    Private Sub DrawAndComputeFingersNum(rect As Rectangle, ByRef img As Image(Of Gray, Byte), index As Integer)
        fingerNum = 0
        If defectArray IsNot Nothing Then
            For i As Integer = 0 To defectArray.Count - 1
                Dim startPoint As PointF = New PointF(CSng(defectArray(i).StartPoint.X), CSng(defectArray(i).StartPoint.Y))
                Dim depthPoint As PointF = New PointF(CSng(defectArray(i).DepthPoint.X), CSng(defectArray(i).DepthPoint.Y))
                Dim endPoint As PointF = New PointF(CSng(defectArray(i).EndPoint.X), CSng(defectArray(i).EndPoint.Y))
                Dim startDepthLine As LineSegment2D = New LineSegment2D(defectArray(i).StartPoint, defectArray(i).DepthPoint)
                Dim depthEndLine As LineSegment2D = New LineSegment2D(defectArray(i).DepthPoint, defectArray(i).EndPoint)
                Dim startCircle As CircleF = New CircleF(startPoint, 1.0F)
                Dim depthCircle As CircleF = New CircleF(depthPoint, 1.0F)
                Dim endCircle As CircleF = New CircleF(endPoint, 1.0F)

                If (startCircle.Center.Y < box(index).center.Y OrElse depthCircle.Center.Y < box(index).center.Y) AndAlso (startCircle.Center.Y < depthCircle.Center.Y) AndAlso (Math.Sqrt(Math.Pow(startCircle.Center.X - depthCircle.Center.X, 2) + Math.Pow(startCircle.Center.Y - depthCircle.Center.Y, 2)) > box(index).size.Height / 6.5) Then
                    fingerNum += 1
                    'MsgBox(fingerNum)
                    currentFrame.Draw(startDepthLine, New Bgr(Color.Green), 2)
                End If

                currentFrame.Draw(startCircle, New Bgr(Color.Red), 2)
                currentFrame.Draw(depthCircle, New Bgr(Color.Yellow), 5)

            Next

            Dim font As MCvFont = New MCvFont(Emgu.CV.CvEnum.FONT.CV_FONT_HERSHEY_DUPLEX, 5.0R, 5.0R)
            If index = 0 Then currentFrame.Draw(fingerNum.ToString(), font, New Point(50, 150), New Bgr(Color.White))

            'Dim bmp = currentFrame.Bitmap
            ' Create a blank bitmap with the same dimensions
            Dim tempBitmap As Bitmap = New Bitmap(img.Width, img.Height)

            Using gra As Graphics = Graphics.FromImage(tempBitmap)
                gra.DrawImage(img.Bitmap, 0, 0)
                Dim br As New SolidBrush(Color.Black)
                gra.FillRectangle(br, rect)
            End Using
            img = New Image(Of Gray, Byte)(tempBitmap)
        End If

    End Sub
    Private Function stretched(ByVal p0 As System.Drawing.Point, ByVal pb As PictureBox) As PointF
        If pb.Image Is Nothing Then Return PointF.Empty
        Dim scaleX As Single = 1.0F * pb.Image.Width / pb.ClientSize.Width
        Dim scaleY As Single = 1.0F * pb.Image.Height / pb.ClientSize.Height
        Return New PointF(p0.X * scaleX, p0.Y * scaleY)
    End Function
    Public Sub DrawLineInt(ByVal bmp As Bitmap, pc As Color, size As Single, x1 As Integer, y1 As Integer, x2 As Integer, y2 As Integer)
        Dim blackPen As Pen = New Pen(pc, size)

        Using g = Graphics.FromImage(bmp)
            g.DrawLine(blackPen, x1, y1, x2, y2)
        End Using
    End Sub

    Private Function CropImage(ByVal source As System.Drawing.Image, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer) As Bitmap
        Try
            Dim crop As Rectangle = New Rectangle(x, y, width, height)
            Dim bmp = New Bitmap(crop.Width, crop.Height)

            Using gr = Graphics.FromImage(bmp)
                gr.DrawImage(source, New Rectangle(0, 0, bmp.Width, bmp.Height), crop, GraphicsUnit.Pixel)
            End Using

            Return bmp
        Catch ex As Exception
            Return Nothing
        End Try
    End Function
    Private Function SetColorImage(ByVal source As System.Drawing.Image, c As Color, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer) As Bitmap
        Dim crop As Rectangle = New Rectangle(x, y, width, height)
        Dim bmp = New Bitmap(crop.Width, crop.Height)
        Dim p As New Pen(c)
        Using gr = Graphics.FromImage(bmp)
            gr.DrawRectangle(p, crop)
        End Using
        Return bmp
    End Function

    Private Function GetRectsFromPic(bmp As Bitmap)
        Dim image As Bitmap = bmp.Clone()
        Dim blobCounter As BlobCounter = New BlobCounter
        With blobCounter
            .MinWidth = 5
            .MinHeight = 5
            .FilterBlobs = True
            .ObjectsOrder = ObjectsOrder.Size
        End With
        Dim objectsData = image.LockBits(New Rectangle(0, 0, image.Width, image.Height), ImageLockMode.ReadOnly, image.PixelFormat)
        Dim grayscaleFilter = New Grayscale(0.2125, 0.7154, 0.0721)
        grayscaleFilter.Apply(New UnmanagedImage(objectsData))
        image.UnlockBits(objectsData)

        blobCounter.ProcessImage(image)
        Dim rects = blobCounter.GetObjectsRectangles()
        Return rects

    End Function

    'Dim face As CascadeClassifier = New CascadeClassifier("C:\Users\PetterPet\source\repos\FaceRegconition1\" + "haarcascade_frontalface_alt2.xml")
    'Dim haar = New CascadeClassifier("C:\Users\PetterPet\source\repos\FaceRegconition1\" + "haarcascade_frontalface_alt2.xml")
    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        'Me.Text = fingerNum.ToString
        'Get the current frame form capture device
        currentFrame = grabber.QueryFrame().Resize(360, 240, Emgu.CV.CvEnum.INTER.CV_INTER_CUBIC).Flip(CvEnum.FLIP.HORIZONTAL)

        'Dim img As Image(Of Bgr, Byte) = currentFrame

        'Dim grayImg As Image(Of Gray, Byte) = img.Convert(Of Gray, Byte)

        'PictureBox5.Image = grayImg.Bitmap

        ''grayImg = img.InRange(New Bgr(100, 0, 0), New Bgr(255, 255, 100))

        'grayImg = img.InRange(New Bgr(bc.B - 25, bc.G - 25, bc.R - 25), New Bgr(bc.B + 25, bc.G + 25, bc.R + 25))

        'Dim Filter As HSLFiltering = New HSLFiltering()
        'Dim sa As Double
        'Dim lu As Double
        'Dim hu As Double
        'RgbToHls(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value, hu, lu, sa)
        'Filter.Saturation = New AForge.Range(sa, sa + 0.5)
        'Filter.Luminance = New AForge.Range(lu, lu + 0.5)
        'Filter.Hue = New AForge.IntRange(hu, hu + 0.5) 'these settings should works, If Not
        'Dim red As Bitmap = Filter.Apply(currentFrame.Bitmap)   'search "HSL color picker online" to tune it

        Dim filteredBitmap = currentFrame.Bitmap.EuclideanFilter(Color.FromArgb(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value))
        Dim filteredBitmapC = currentFrame.Bitmap.EuclideanFilter(Color.FromArgb(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value))
        'Dim filteredBitmap = red.EuclideanFilter(Color.FromArgb(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value))
        Dim processedFilteredBitmap = filteredBitmap.FindObjectsOnFiltered(Color.Red, multiple:=True)
        Dim idk As Rectangle() = GetRectsFromPic(filteredBitmap)
        Dim rect_12 As StructuringElementEx = New StructuringElementEx(3, 3, 1, 1, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_RECT)
        CvInvoke.cvErode(currentFrame, currentFrame, rect_12, 1)
        Dim rect_6 As StructuringElementEx = New StructuringElementEx(1, 1, 0, 0, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_RECT)
        CvInvoke.cvDilate(currentFrame, currentFrame, rect_6, 2)
        If currentFrame IsNot Nothing And idk.Count > 0 Then
            Try
                Dim smth = currentFrame.Clone
                smth.Draw(idk(0), New Bgr(Color.Blue), 10)
                'PictureBox2.Image = smth.Bitmap
            Catch ex As Exception
            End Try
        End If

        'Dim img As Image(Of Bgr, Byte) = currentFrame
        'Dim imgCANNY As Image(Of Gray, Byte) = New Image(Of Gray, Byte)(img.Bitmap.EuclideanFilter(Color.FromArgb(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value)))
        'Dim img_BINARY As Image(Of Bgr, Byte) = img.ThresholdBinary(New Bgr(100, 100, 0), New Bgr(0, 100, 100))
        'PictureBox2.Image = img_BINARY.Bitmap

        'Dim testImg As Image(Of Bgr, Byte) = currentFrame
        ''MASK TO BE USED FOR FLOODFILL MUST BE 2 PIXELS WIDER & HIGHER
        'Dim mask As Image(Of Gray, Byte) = New Image(Of Gray, Byte)(testImg.Width + 2, testImg.Height + 2)
        'Dim comp As MCvConnectedComp = New MCvConnectedComp()
        ''SEED POINT LOCATION IN IMAGE (I CHOOSE MIDPOINT OF IMAGE AS FOREGROUND OBJECT)
        'Dim point1 As Point = New Point(testImg.Width / 2, testImg.Height / 2)

        ''CREATE MASK FROM REGION GROWING
        'CvInvoke.cvFloodFill(testImg, point1, New MCvScalar(255), New MCvScalar(50, 30, 10), New MCvScalar(50, 30, 10), comp, 8 Or (1 <<17) Or (255 <<8), mask)

        ''//RESIZING MASK TO 2PIXELS LESSER IN WIDTH & HEIGHT
        'Dim reducedMask As Image(Of Gray, Byte) = mask.Copy(New Rectangle(1, 1, testImg.Width, testImg.Height))
        'PictureBox2.Image = reducedMask.Bitmap

        'Dim smoothedFrame As MCvMat = New MCvMat()
        'CvInvoke.GaussianBlur(img, smoothedFrame, New Size(3, 3), 1)
        'Dim img3 = img2gray.AbsDiff(smoothedFrame.ToImage < gray, Byte > ())
        'img3 = img3.ThresholdBinary(New Gray(60), New Gray(255))
        'Dim jwoifj As Emgu.CV.VideoSurveillance.
        'Dim processedFilteredBitmapBGR = New Image(Of Bgr, Byte)(processedFilteredBitmap)
        'Dim rect_12 As StructuringElementEx = New StructuringElementEx(3, 3, 1, 1, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_RECT)
        'CvInvoke.cvErode(processedFilteredBitmapBGR, processedFilteredBitmapBGR, rect_12, 1)
        'Dim rect_6 As StructuringElementEx = New StructuringElementEx(1, 1, 0, 0, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_RECT)
        'CvInvoke.cvDilate(processedFilteredBitmapBGR, processedFilteredBitmapBGR, rect_6, 2)

        PictureBox3.Image = processedFilteredBitmap
        Dim o As Image(Of Bgr, Byte) = New Image(Of Bgr, Byte)(filteredBitmapC)
        Dim c As Bitmap = filteredBitmapC.Clone
        Dim fjwierjf = New Image(Of Bgr, Byte)(c)
        Dim des As Image(Of Gray, Byte) = o.Convert(Of Gray, Byte)
        CvInvoke.cvCvtColor(o, des, Emgu.CV.CvEnum.COLOR_CONVERSION.CV_RGB2GRAY)
        Dim contours As Contour(Of Point) = des.FindContours(CvEnum.CHAIN_APPROX_METHOD.CV_CHAIN_APPROX_SIMPLE, CvEnum.RETR_TYPE.CV_RETR_LIST)

        Dim lstRectangles As List(Of MCvBox2D) = New List(Of MCvBox2D)()    'declare list of rectangles

        'CvInvoke.cvDrawContours(o, contours, New MCvScalar(0, 0, 0), New MCvScalar(0, 0, 0), -1, 2, Emgu.CV.CvEnum.LINE_TYPE.EIGHT_CONNECTED, New Point(0, 0))
        'Dim acontours() As Point = contours.ToArray
        'Me.Text = acontours.Count
        'For Each pnt In acontours
        '    Dim g4 As Graphics = Graphics.FromImage(o.Bitmap)
        '    g4.DrawEllipse(New Pen(Color.Red), New Rectangle(pnt.X, pnt.Y, 20, 20))
        'Next
        'Dim currentContour = contours.ApproxPoly(contours.Perimeter * 0.015)
        'CvInvoke.cvDrawContours(des, contours, New MCvScalar(255), New MCvScalar(255), -1, 2, Emgu.CV.CvEnum.LINE_TYPE.EIGHT_CONNECTED, New Point(0, 0))
        'Dim grayColor As New Emgu.CV.Structure.Gray
        'des.Draw(currentContour.BoundingRectangle, grayColor, 100)
        'PictureBox2.Image = des.Bitmap



        While (Not contours Is Nothing)
            Dim contour As Contour(Of Point) = contours
            If (contour.Area > 250) Then      'avoid small spots
                Dim ptPoints() As Point = contour.ToArray()             'get contour points
                Dim blnIsRectangle As Boolean = True

                If (blnIsRectangle) Then
                    lstRectangles.Add(contour.GetMinAreaRect()) 'Add the rectangles to the image
                End If
            End If
            contours = contours.HNext
        End While
        Dim boxLength As Double
        Dim boxWidth As Double


        For Each rect As MCvBox2D In lstRectangles
            o.Draw(rect, New Bgr(Color.Red), 2)
            boxLength = rect.size.Width
            boxWidth = rect.size.Height
            PictureBox2.Image = o.Bitmap
        Next
        'Dim filteredBitmap2 = currentFrame.Bitmap.EuclideanFilter(Color.FromArgb(TrackBar4.Value, TrackBar5.Value, TrackBar6.Value))
        'Dim processedFilteredBitmap2 = filteredBitmap2.FindObjectsOnFiltered(Color.Red, multiple:=True)
        'Dim idk2 As Rectangle() = GetRectsFromPic(filteredBitmap2)

        'If currentFrame IsNot Nothing And idk2.Count > 0 Then
        '    Try
        '        Dim smth = currentFrame.Clone
        '        smth.Draw(idk2(0), New Bgr(Color.Blue), 10)
        '        PictureBox5.Image = smth.Bitmap
        '    Catch ex As Exception
        '    End Try
        'End If
        'PictureBox6.Image = processedFilteredBitmap2

        'Convert it to Grayscale
        gray = currentFrame.Convert(Of Gray, [Byte])()
        'Face Detector
        Dim facesDetected As MCvAvgComp()() = gray.DetectHaarCascade(face, 1.2, 10, Emgu.CV.CvEnum.HAAR_DETECTION_TYPE.DO_CANNY_PRUNING, New Size(20, 20))
        'Me.Text = facesDetected(0).Count
        'Action for each element detected
        For Each f As MCvAvgComp In facesDetected(0)
            result = currentFrame.Copy(f.rect).Convert(Of Gray, Byte)().Resize(100, 100, Emgu.CV.CvEnum.INTER.CV_INTER_CUBIC)
            'draw the face detected in the 0th (gray) channel with blue color
            currentFrame.Draw(f.rect, New Bgr(Color.Red), 2)
            'Me.Text = f.rect.X + f.rect.Width / 2 - 10 & "-" & f.rect.Y + f.rect.Height / 2 - 10
            Dim rect As New Rectangle(f.rect.X + f.rect.Width / 2 - 10, f.rect.Y + f.rect.Height / 2 - 10, 20, 20)
            currentFrame.Draw(rect, New Bgr(Color.Red), 2)

            Dim lol = New Image(Of Bgr, [Byte])(currentFrame.Bitmap.EuclideanFilter(Color.FromArgb(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value)))
            'PictureBox5.Image = currentFrame.Bitmap.EuclideanFilter(Color.FromArgb(TrackBar1.Value, TrackBar2.Value, TrackBar3.Value))

            Using g As Graphics = Graphics.FromImage(currentFrame.Bitmap)
                Dim sb As New SolidBrush(Color.Black)
                g.FillRectangle(sb, f.rect)
            End Using

            Using gra As Graphics = Graphics.FromImage(filteredBitmapC)
                Dim br As New SolidBrush(Color.Black)
                gra.FillRectangle(br, f.rect)
            End Using

            'Me.Text = handsDetected(0).Count
            Dim b As New Bitmap(PictureBox2.Width, PictureBox2.Height)
            DrawLineInt(b, Color.Black, f.rect.Width / 6, f.rect.X + f.rect.Width / 2, f.rect.Y, f.rect.X + f.rect.Width / 2, b.Height)
            Dim g2 As Graphics = Graphics.FromImage(b)
            Dim sb2 As New SolidBrush(Color.Black)
            g2.FillEllipse(sb2, f.rect)
            If CheckBox1.Checked Then
                Dim handsDetected As MCvAvgComp()() = lol.DetectHaarCascade(hand, 1.2, 10, Emgu.CV.CvEnum.HAAR_DETECTION_TYPE.DO_CANNY_PRUNING, New Size(20, 20))
                For Each h As MCvAvgComp In handsDetected(0)
                    currentFrame.Draw(h.rect, New Bgr(Color.Red), 2)
                    DrawLineInt(b, Color.Red, 10, f.rect.X + f.rect.Width / 2, f.rect.Y + f.rect.Height, h.rect.X + h.rect.Width / 2, h.rect.Y + h.rect.Height / 2)
                    'Dim handR As New Rectangle
                    'handR.X = h.rect.X + h.rect.Width / 2
                    'handR.Y = h.rect.Y + h.rect.Height / 2
                    'handR.Width = 60
                    'handR.Height = 25
                    'Dim g3 As Graphics = Graphics.FromImage(b)
                    'Dim sb3 As New SolidBrush(Color.DarkBlue)
                    'g3.FillEllipse(sb3, handR)
                    'If index = 1 Then DrawLineInt(b, Color.DarkBlue, f.rect.X + f.rect.Width / 2, f.rect.Y + f.rect.Height, h.rect.X + h.rect.Width / 2, h.rect.Y + h.rect.Height / 2)
                Next
            ElseIf CheckBox2.Checked Then
                Dim str As String = ""
                For i = 0 To CDHandPoints.Count - 1
                    Dim p = CDHandPoints(i)
                    DrawLineInt(b, Color.Red, 10, f.rect.X + f.rect.Width / 2, f.rect.Y + f.rect.Height, p.X, p.Y)
                    str += New Point(p.X, p.Y + box(i).size.Height / 2).ToString

                Next
                If CDHandPoints(0).Y + box(0).size.Height / 2 >= PictureBox9.Location.Y Then
                    PictureBox9.Width = CDHandPoints(0).X - PictureBox9.Location.X
                End If
                Me.Text = str

            End If
            'PictureBox13.Location = New Point(box(0).center.X - box(0).size.Width / 2 + PictureBox12.Left, box(0).center.Y - box(0).size.Height / 2 + PictureBox12.Top)
            PictureBox13.Left = box(0).center.X - box(0).size.Width / 2 + PictureBox12.Left

            PictureBox7.Image = b
        Next
        'Show the faces procesed and recognized
        Dim grayFilteredBitmapC = New Image(Of Gray, Byte)(filteredBitmapC)
        CvInvoke.cvSmooth(grayFilteredBitmapC, grayFilteredBitmapC, CvEnum.SMOOTH_TYPE.CV_BLUR, 4, 4, 0, 0)
        CvInvoke.cvThreshold(grayFilteredBitmapC, grayFilteredBitmapC, 70, 255, CvEnum.THRESH.CV_THRESH_BINARY_INV)
        grayFilteredBitmapC = grayFilteredBitmapC.Not
        'CvInvoke.cvInvert(grayFilteredBitmapC, grayFilteredBitmapC, CvEnum.INVERT_METHOD.CV_SVD_SYM)
        Dim rect2_12 As StructuringElementEx = New StructuringElementEx(2, 2, 1, 1, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_RECT)
        CvInvoke.cvErode(grayFilteredBitmapC, grayFilteredBitmapC, rect2_12, 1)
        Dim rect2_6 As StructuringElementEx = New StructuringElementEx(4, 4, 2, 2, Emgu.CV.CvEnum.CV_ELEMENT_SHAPE.CV_SHAPE_ELLIPSE)
        CvInvoke.cvDilate(grayFilteredBitmapC, grayFilteredBitmapC, rect2_6, 2)
        PictureBox10.Image = CropImage(grayFilteredBitmapC.Bitmap, box(0).center.X - box(0).size.Width / 2, box(0).center.Y - box(0).size.Height / 2, box(0).size.Width, box(0).size.Height)
        PictureBox11.Image = CropImage(grayFilteredBitmapC.Bitmap, box(1).center.X - box(1).size.Width / 2, box(1).center.Y - box(1).size.Height / 2, box(1).size.Width, box(1).size.Height)
        PictureBox5.Image = grayFilteredBitmapC.Bitmap
        ExtractContourAndHull(grayFilteredBitmapC, 0)
        ExtractContourAndHull(grayFilteredBitmapC, 1)
        PictureBox8.Image = grayFilteredBitmapC.Bitmap
        PictureBox1.Image = currentFrame.Bitmap
        If facesDetected(0).Count <> 0 Then
            Dim r As Rectangle = facesDetected(0)(0).rect
            'PictureBox3.Image = CropImage(currentFrame.Bitmap, r.Location.X, r.Location.Y, r.Width, r.Height)
        End If

        'Dim Image As IInputArray = grabber.QueryFrame()
        'Dim currentFrame As Image(Of Bgr, [Byte])

        'Using ugray As UMat = New UMat()

        '    CvInvoke.CvtColor(Image, ugray, Emgu.CV.CvEnum.ColorConversion.Bgr2Gray)

        '    '//normalizes brightness And increases contrast of the image
        '    CvInvoke.EqualizeHist(ugray, ugray)

        '    '//Detect the faces  from the gray scale image And store the locations as rectangle                   
        '    Dim facesDetected As Rectangle() = face.DetectMultiScale(
        '   ugray, 1.1, 10, New Size(20, 20))
        '    currentFrame = New Image(Of Bgr, [Byte])(grabber.QueryFrame().Bitmap)
        '    For Each f In facesDetected
        '        currentFrame.Draw(f, New Bgr(Color.Red), 2)
        '    Next
        '    PictureBox1.Image = currentFrame.Bitmap
        'End Using

        'Dim matFrame As Mat = grabber.QueryFrame()
        'Dim nextFrame As Image(Of Bgr, [Byte]) = matFrame.ToImage(Of Bgr, [Byte])
        'Dim grayframe As Image(Of Bgr, [Byte]) = nextFrame.Convert(Of Bgr, [Byte])

        'Dim faces = haar.DetectMultiScale(matFrame, 1.1, 10,
        '          New Size(20, 20))
        'Dim currentFrame As Image(Of Bgr, [Byte])
        'currentFrame = New Image(Of Bgr, [Byte])(grabber.QueryFrame().Bitmap)
        'For Each f In faces
        '    currentFrame.Draw(f, New Bgr(Color.Red), 2)
        'Next
        'PictureBox1.Image = currentFrame.Bitmap
    End Sub

    Private Sub PictureBox1_MouseMove(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseMove
        'Me.Text = e.Location.ToString
        'Using snoop = New BmpPixelSnoop(PictureBox1.Image)
        '    PictureBox3.BackColor = snoop.GetPixel(e.Location.X, e.Location.Y)
        'End Using
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        iColorToSet = 0
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        iColorToSet = 1
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        PictureBox1.Visible = False
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        If CheckBox2.Checked Then CheckBox1.Checked = False

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If CheckBox1.Checked Then CheckBox2.Checked = False
    End Sub

    Private Sub PictureBox1_MouseClick(sender As Object, e As MouseEventArgs) Handles PictureBox1.MouseClick
        Dim mDown As Point = System.Drawing.Point.Round(stretched(e.Location, PictureBox1))
        Dim c As Color = (CType(PictureBox1.Image, Bitmap)).GetPixel(mDown.X, mDown.Y)
        bc = c
        PictureBox4.BackColor = bc
        Select Case iColorToSet
            Case 0
                TrackBar1.Value = bc.R
                TrackBar2.Value = bc.G
                TrackBar3.Value = bc.B
            Case 1
                TrackBar4.Value = bc.R
                TrackBar5.Value = bc.G
                TrackBar6.Value = bc.B
        End Select
    End Sub
End Class
