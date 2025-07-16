% Open an ActiveX connection to Excel
Excel = actxserver('Excel.Application');
Excel.Visible = 1; % Make Excel visible.

% Open the workbook
wb1 = Excel.Workbooks.Open('C:\Users\file_location\Refinery Capacity Projection Analysis.xlsm');

% Access the worksheet
ResultTab = wb1.Sheets.Item('RefCapacityProj');

% Determine the last row
lastRow = ResultTab.Range('A2').End('xlDown').Row;

% Loop over the rows
for i = 2 :lastRow
    % Define the initial values, lb and ub for each row
    initial_values = cell2mat(ResultTab.Range(['AN' num2str(i) ':AT' num2str(i)]).Value);
    lb = cell2mat(ResultTab.Range(['BB' num2str(i) ':BH' num2str(i)]).Value);
    ub = cell2mat(ResultTab.Range(['AU' num2str(i) ':BA' num2str(i)]).Value);

    % Define the objective function and constraint for each row
    objective_function = @(values) my_objective_function(values, ResultTab, i);
    nonlcon = @(values) my_constraint(values, ResultTab, i);

    % Run the single-objective optimization with the nonlinear constraint
    options = optimoptions('fmincon','Display','iter');
    [optimized_values, ~, ~, ~] = fmincon(objective_function, initial_values, [], [], [], [], lb, ub, nonlcon, options);

    % Write the optimized values back to Excel
    for j = 1:length(optimized_values)
        cell = ['A' char('G' + (j-1)) num2str(i)];  % Creates strings like 'AG2', 'AH2', etc.
        ResultTab.Range(cell).Value = optimized_values(j);
    end
end 

% Save and close the workbook
wb1.Save;

% Define the single-objective function with Excel update
function objective = my_objective_function(values, ResultTab, i)
    % Update Excel with the current values
    for j = 1:length(values)
        cell = ['A' char('G' + (j-1)) num2str(i)];  % Adjust cell references as needed
        ResultTab.Range(cell).Value = values(j);
    end
    % Recalculate the workbook (if needed, depends on Excel setup)
    ResultTab.Application.Calculate;
    
    % Read the updated result
    objective = ResultTab.Range(['BK' num2str(i)]).Value;
end

% Define the nonlinear constraint with Excel update
function [c1, c2, ceq] = my_constraint(values, ResultTab, i)
    % Assuming the same cells need to be updated for constraints
    for j = 1:length(values)
        cell = ['A' char('G' + (j-1)) num2str(i)];  % Adjust cell references as needed
        ResultTab.Range(cell).Value = values(j);
    end
    % Recalculate the workbook (if needed, depends on Excel setup)
    ResultTab.Application.Calculate;
    
    % Read the updated constraint value
    constraint_value_BJ = ResultTab.Range(['BJ' num2str(i)]).Value;
    constraint_value_BK = ResultTab.Range(['BK' num2str(i)]).Value;
    % Define the inequality constraint (c<=0); BJ cells are the absolute value of
    % the differences between the calculated and expected crude input.
    c1 = constraint_value_BJ - 0.01*ResultTab.Range(['Y' num2str(i)]).Value;
    c2 = constraint_value_BK - 0.01;
    % Define the equality constraint (none in this case)
    ceq = [];
end
