#include <stdio.h>
#include <stdlib.h>
#include <Python.h>



PyObject *pUpdatePlotData = NULL;
PyObject *pSetGrid = NULL;

int initialise_python()
{

    PyObject *pName, *pModule, *pInitContourPlot;
    PyObject *pArgs, *pValue;
    int i;


    printf("Initialising python\n");
    Py_Initialize();
    Py_SetPath(L".");
    pName = PyUnicode_FromString("contour_plot");
    printf("pName: %p\n", pName);

    /* Error checking of pName left out */
    pModule = PyImport_Import(pName);
    Py_DECREF(pName);
    printf("pModule: %p\n", pModule);

    if (pModule == NULL)
    {
        fprintf(stderr, "Cannot import contour_plot - check PYTHONPATH.\n");
        exit(-1);
    }
    pInitContourPlot = PyObject_GetAttrString(pModule, "init_contour_plot");
    pSetGrid = PyObject_GetAttrString(pModule, "set_grid");
    pUpdatePlotData = PyObject_GetAttrString(pModule, "update_plot_data");
    pArgs = PyTuple_New(0);
    pValue = PyObject_CallObject(pInitContourPlot, pArgs);
}   // initialise_python

// ----------------------------------------------------------------------------
int set_grid(int *n_chi, double *chi1, double *chi2, double *chi3)
{
    PyObject *l1 = PyList_New(*n_chi);
    PyObject *l2 = PyList_New(*n_chi);
    PyObject *l3 = PyList_New(*n_chi);
    for(int i=0; i<*n_chi; i++)
    {
        PyList_SET_ITEM(l1, i, PyFloat_FromDouble(chi1[i]));
        PyList_SET_ITEM(l2, i, PyFloat_FromDouble(chi2[i]));
        PyList_SET_ITEM(l3, i, PyFloat_FromDouble(chi3[i]));
    }

    PyObject *pArgs = PyTuple_New(3);
    PyTuple_SetItem(pArgs, 0, l1);
    PyTuple_SetItem(pArgs, 1, l2);
    PyTuple_SetItem(pArgs, 2, l3);
    PyObject *pValue = PyObject_CallObject(pSetGrid, pArgs);
}   // set_grid

// ----------------------------------------------------------------------------

int update_plot_data(int *n_field, double *field)
{
    if (!pUpdatePlotData)
    {
        fprintf(stderr, "Python not properly initialised.");
        exit(-2);
    }

    PyObject *l = PyList_New(*n_field);
    for(int i=0; i<*n_field; i++)
    {
        PyList_SET_ITEM(l, i, PyFloat_FromDouble(field[i]));
    }

    PyObject *pArgs = PyTuple_New(1);
    PyTuple_SetItem(pArgs, 0, l);
    PyObject *pValue = PyObject_CallObject(pUpdatePlotData, pArgs);

    return 0;
}   //update_plot_data