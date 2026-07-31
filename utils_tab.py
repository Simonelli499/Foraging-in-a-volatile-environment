

def dataframe_to_latex_table(df):

    col_num = len(df.columns)
    col_format = 'l' + ''.join(['c'] * (col_num-1))

    s = '\\begin{table}[htb]\n'
    s += '\\centering\n'
    s += '\\begin{tabular}{' + col_format + '}\n'
    s += '\\toprule\n'

    first_row = ''

    for i, col in enumerate(df):
        if i > 0:
            first_row += ' & '
        first_row += col

    s += first_row + "\\\\\n"
    s += '\\hline\n'

    for index, row in df.iterrows():

        for i, col in enumerate(row):
            if i > 0:
                s += ' & '

            col_name = df.columns[i]

            if isinstance(col, float):
                if col_name == 'p-value':
                    if col < 0.001:
                        s += '\\bm{$< 0.001$}'
                    else:
                        s += f'{col:.1f}'
                else:
                    s += f'{col:.1f}'
            else:
                s += col

            # print(col)
        s += "\\\\\n"

    s += '\\bottomrule\n'

    s += '\\end{tabular}\n'
    s += '\\caption{}\n'
    s += '\\label{tab:}\n'
    s += '\\end{table}\n'

    return s


def lmm_to_latex_table(df_fixed, random_var=None):

    col_fixed_num = len(df_fixed.columns) - 1
    col_format = 'l' + ''.join(['c'] * (col_fixed_num))

    s = '\\begin{table}[htb]\n'
    s += '\\centering\n'
    s += '\\begin{tabular}{' + col_format + '}\n'
    s += '\\toprule\n'

    # Prima riga con i fixed e random effects
    # s += ' & \\multicolumn{' + f'{col_fixed_num-1}' + '}{c}{Fixed effects}\\\\\n'
    # s += ' & \\multicolumn{1}{c}{Random effects}\\\\\n'
    # s += '\\cmidrule(lr){2-' + f'{col_fixed_num}' + '}'
    # s += '\\cmidrule(lr){' + f'{col_fixed_num+1}' + '-' + f'{col_fixed_num + 1}' + '}\n'

    for i, col in enumerate(df_fixed):
        if i > 0:
            s += ' & '
        s += col

    # s += ' & Variance'
    s += "\\\\\n"
    s += '\\midrule\n'

    for index, row in df_fixed.iterrows():

        for i, col in enumerate(row):
            if i > 0:
                s += ' & '

            col_name = df_fixed.columns[i]

            if isinstance(col, float):
                if col_name == 'p-value':
                    if col < 0.001:
                        s += '\\bm{$< 0.001$}'
                    else:
                        s += f'{col:.3f}'
                else:
                    s += f'{col:.3f}'
            else:
                s += col

        s += "\\\\\n"

    if random_var is not None:

        s += '\\midrule\n'
        s += f'Group Variance & {random_var:.3f} \\\\\n'

    s += '\\bottomrule\n'
    s += '\\end{tabular}\n'
    s += '\\caption{}\n'
    s += '\\label{tab:}\n'
    s += '\\end{table}\n'

    return s
