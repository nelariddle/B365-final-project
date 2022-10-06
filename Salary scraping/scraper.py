from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.select import Select
from bs4 import BeautifulSoup
import csv
import time


PATH = "C:\Program Files (x86)\chromedriver.exe"
driver = webdriver.Chrome(PATH)

csv_filename = "salaries.csv"

col_names_done = False

for year in range(1995, 2023):
    driver.get("https://ops.fms.iu.edu/psgi/Salary/")
    if not col_names_done:
        username = driver.find_element(By.ID, "username")
        password = driver.find_element(By.ID, "password")
        # REDACT
        username.send_keys("REDACTED")
        password.send_keys("REDACTED")

        driver.find_element(By.NAME, "_eventId_proceed").click()
        driver.implicitly_wait(500)

    funding_org_select = driver.find_element(
        By.XPATH, "//input[@name='org_cd']")
    funding_org_select.send_keys("csci")

    year_select = Select(driver.find_element(
        By.ID, "univ_fiscal_yr"))
    year_select.select_by_visible_text(str(year)+"-"+str(year+1))

    driver.find_element(By.XPATH, "//input[@type='submit']").click()

    page = driver.page_source
    soup = BeautifulSoup(page, "html.parser")
    if not col_names_done:
        col_names = [child.get_text().split("\n")
                     for child in soup.find_all("thead")[0].children if not child == "\n"][0]
        col_names.insert(0, "Year")
        with open(csv_filename, "w", newline='') as csv_file:
            writer = csv.writer(csv_file)
            writer.writerow(col_names)
        col_names_done = True
    for row in soup.find_all(
            "thead")[0].find_next_siblings():
        for child in row.children:
            row_data = [year]
            for element in child:
                if not element == "\n":
                    row_data.append(element.get_text())
            if not len(row_data) == 1:
                with open(csv_filename, "a", newline='') as csv_file:
                    writer = csv.writer(csv_file)
                    writer.writerow(row_data)
