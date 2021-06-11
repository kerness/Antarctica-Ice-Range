echo "Cerating video from Antarctica polar charts..."
ffmpeg -start_number 1 -i ../charts/resModel/fig_%d.png -qscale 0 ../results/video/iceRange-resModel.mp4

ffmpeg -start_number 1 -i ../charts/resInterpolation/fig_%d.png -qscale 0 ../results/video/iceRange-resInterpolation.mp4
