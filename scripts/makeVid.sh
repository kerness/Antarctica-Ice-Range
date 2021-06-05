echo "Cerating video from Aantarctica polar charts..."
ffmpeg -start_number 1 -i ../charts/combinePlots/figTitle_%d.png -qscale 0 ../results/iceRange.mp4
