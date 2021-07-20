from django.http import HttpResponse
from django.shortcuts import render
import sys
sys.path.append('./scripts')
from chart_seasonality_example import rendering_charts


def home_page(request):
    if request.method == 'GET':
        print("ˆˆˆˆˆˆˆˆˆˆˆˆˆˆHomepageˆˆˆˆˆˆˆˆˆˆˆˆ")
    return render(request, "home.html")


def get_new_charts(request):
    if request.method == 'POST':
        selected_ticker = request.POST.get('ticker')
        daily_img_path = "reports/charts/seasonality_daily_{selected_ticker}_2011-01-01_2020-12-31.jpg".format(selected_ticker=selected_ticker)
        monthly_img_path = "reports/charts/seasonality_monthly_{selected_ticker}_2011-01-01_2020-12-31.jpg".format(selected_ticker=selected_ticker)
        rendering_charts(selected_ticker)
        print(f"********** Getting {selected_ticker} charts **********")   
        return render(request, "charts.html", {"daily_img_path": daily_img_path, "monthly_img_path": monthly_img_path})
    