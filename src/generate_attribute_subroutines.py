import string

# Define the type-kind and HDF5 types for each data type
data_types = {'short_integer': ('SNK', 'H5T_NATIVE_INTEGER'),
              'long_integer': ('SLK', 'H5T_STD_I64LE'),
              'short_real': ('SSK', 'H5T_NATIVE_REAL'),
              'long_real': ('SDK', 'H5T_NATIVE_DOUBLE')}

# Define the dimensions for the arrays
array_dims = ['0', '1', '2', '3', '4', '5', '6', '7']

# Define the string templates for the subroutines
sub_templates = {'scalar': string.Template('''\
SUBROUTINE write_attribute_${dtype}_0(this, obj_name, attr_name, attr_val)
  CLASS(HDF5FileType), INTENT(INOUT) :: this
  CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
  ${dtype}, INTENT(IN) :: attr_val

#ifdef FUTILITY_HAVE_HDF5
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

  dims = 1

  CALL preWrite_attribute(this, dims, obj_name, attr_name, ${hdf5_type}, &
      obj_id, dspace_id, attr_id)
  CALL h5awrite_f(attr_id, ${hdf5_type}, attr_val, dims, error)
  CALL postWrite_attribute(this, dspace_id, attr_id, obj_id)

#endif
END SUBROUTINE write_attribute_${dtype}_0
'''),

                 'array': string.Template('''\
SUBROUTINE write_attribute_${dtype}_$dim(this, obj_name, attr_name, attr_val)
  CLASS(HDF5FileType), INTENT(INOUT) :: this
  CHARACTER(LEN=*), INTENT(IN) :: obj_name, attr_name
  ${dtype}, DIMENSION($dim) :: attr_val

#ifdef FUTILITY_HAVE_HDF5
  INTEGER(HID_T) :: attr_id, dspace_id, obj_id
  INTEGER(HSIZE_T), DIMENSION(1) :: dims

  dims = SHAPE(attr_val)

  CALL preWrite_attribute(this, dims, obj_name, attr_name, ${hdf5_type}, &
      obj_id, dspace_id, attr_id)
  CALL h5awrite_f(attr_id, ${hdf5_type}, attr_val, dims, error)
  CALL postWrite_attribute(this, dspace_id, attr_id, obj_id)

#endif
END SUBROUTINE write_attribute_${dtype}_$dim
''')}

# Generate the Fortran subroutines
print (sub_templates)
sub_file = open(f'write_attribute.f90', 'w')
for dtype, (type_kind, hdf5_type) in data_types.items():
    for scalar_or_array in sub_templates:

        if scalar_or_array == 'scalar':
          # Write the scalar subroutine code
          sub_code = sub_templates[scalar_or_array].substitute(dtype=dtype.upper(),
                                                              type_kind=type_kind,
                                                              hdf5_type=hdf5_type)
          sub_file.write(sub_code)
        else:
           # Write the array subroutine code
           for dim in range(1,8):
            sub_code = sub_templates[scalar_or_array].substitute(dtype=dtype.upper(),
                                                                type_kind=type_kind,
                                                                hdf5_type=hdf5_type,
                                                                dim=dim)
            sub_file.write(sub_code)
