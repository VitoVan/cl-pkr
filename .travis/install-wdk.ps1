# Copied from
# https://github.com/appveyor/build-images/blob/master/scripts/Windows/install_wdk_1809.ps1

Write-Host "Installing Windows SDK for Windows 10.0.15063.468" -ForegroundColor Cyan

Write-Host "Downloading..."
$exePath = "$env:temp\wdksetup.exe"
(New-Object Net.WebClient).DownloadFile('https://go.microsoft.com/fwlink/p/?LinkID=845298', $exePath)

Write-Host "Installing..."
cmd /c start /wait $exePath /features + /quiet

Remove-Item $exePath
Write-Host "Installed" -ForegroundColor Green