import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def plot_networth(df):
    
    df = df.sort_values(df.columns.values[1], ascending=False)
    networths = df["totalNetWorths"]
    rownames = df[df.columns.values[0]]

    index = np.arange(df.shape[0])
    bar_width = 0.35

    opacity = 0.4
    error_config = {'ecolor': '0.3'}

    rect = plt.bar(index + bar_width, networths, bar_width,
                   alpha=opacity,
                   color='r',
                   error_kw=error_config)

    plt.xlabel('Net Worths')
    plt.ylabel('Country') if df.columns.values[0] == "countries" else plt.ylabel("Industry")
    plt.title('Total Billionaires Net Worth By Country') if df.columns.values[0] == "countries" else plt.title("Total Billionaires Net Worth By Industry")
    plt.xticks(index + bar_width / 2, rownames, rotation = 90)
    plt.legend()

    plt.tight_layout()
    plt.show()


def plot_change(df):
    
    df = df.sort_values(df.columns.values[1], ascending=False)
    changes = df[df.columns.values[1]]
    rownames = df[df.columns.values[0]]

    index = np.arange(df.shape[0])
    bar_width = 0.35

    opacity = 0.4
    error_config = {'ecolor': '0.3'}

    rect = plt.bar(index + bar_width, changes, bar_width,
                   alpha=opacity,
                   color='r',
                   error_kw=error_config)
    
    if df.columns.values[0] == "Last_change":
        plt.xlabel('Country')
        plt.ylabel('Last Change')
        plt.title('Total Billionaires Net Worth By Country')
    else:
        plt.ylabel("Industry")
        plt.ylabel('YTD Change')
        plt.title("Total Billionaires Net Worth By Industry")
    plt.xticks(index + bar_width / 2, rownames, rotation = 90)
    plt.legend()

    plt.tight_layout()
    plt.show()